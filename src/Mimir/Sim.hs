{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Mimir.Sim(
    module Mimir.Std,
    Sim,
    createSim,
    destroySim
) where

import Mimir.Types
import Mimir.Std
import Mimir.Sim.Types

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Lens (Lens, over, set, view)
import Control.Monad
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Trans
import Data.Time.Clock.POSIX (getPOSIXTime)

---
--- Create a Sim
---

createSim :: Exchange e => Int -> Double -> Double -> e -> IO (Sim e)
createSim cycleDelayMS currencyBalance commodityBalance e = do
    idg <- fmap round getPOSIXTime
    let stat = SimState {
        _ssIDGen = idg,
        _ssCurrencyBalance = currencyBalance,
        _ssCommodityBalance = commodityBalance,
        _ssPendingLimitOrders = [],
        _ssPendingMarketOrders = []
    }
    tstat <- atomically $ newTVar stat
    tid <- forkIO $ runSim cycleDelayMS tstat
    return $ Sim {
        _siExchange = e,
        _siManagerThreadID = tid,
        _siState = tstat
    }

---
--- Destroy a Sim
---

destroySim :: Sim e -> IO ()
destroySim = killThread . view siManagerThreadID

---
--- Run a Sim
---

runSim :: Int -> TVar SimState -> IO ()
runSim cycleDelayMS tstat = forever $ undefined

---
--- Utility
---

readState :: (Exchange e, MonadIO (ExchangeM e)) => Sim e -> (ExchangeM e) SimState
readState = liftIO . atomically . readTVar . view siState

viewState :: (Exchange e, MonadIO (ExchangeM e)) => Lens SimState SimState r r -> Sim e -> (ExchangeM e) r
viewState lens = liftIO . atomically . fmap (view lens) . readTVar . view siState

operateState :: (Exchange e, MonadIO (ExchangeM e)) => (SimState -> (a, SimState)) -> Sim e -> (ExchangeM e) a
operateState f sim = liftIO . atomically $ do
    let tv = view siState sim
    (res, stat) <- fmap f $ readTVar tv
    writeTVar tv stat
    return res

modifyState :: (Exchange e, MonadIO (ExchangeM e)) => (SimState -> SimState) -> Sim e -> (ExchangeM e) ()
modifyState f = operateState $ (\s -> ((), f s))

instance HasStd Sim where
    getStd = view siExchange

instance Exchange e => Exchange (Sim e) where
    type ExchangeM (Sim e) = ExchangeM e

---
--- Ticker
---

instance (Exchange e, TickerP e) => TickerP (Sim e) where
    type TickerT (Sim e) = TickerT e
    ticker' = ticker' . view siExchange

---
--- Candles
---

instance (Exchange e, CandlesP e) => CandlesP (Sim e) where
    type CandleIntervalT (Sim e) = CandleIntervalT e
    type CandleT (Sim e) = CandleT e
    candles' t iv = candles' (view siExchange t) iv

---
--- OrderBook
---

instance (Exchange e, OrderBookP e) => OrderBookP (Sim e) where
    type OrderBookT (Sim e) = OrderBookT e
    orderBook' = orderBook' . view siExchange

---
--- TradeHistory
---

instance (Exchange e, TradeHistoryP e) => TradeHistoryP (Sim e) where
    type TradeT (Sim e) = TradeT e
    tradeHistory' = tradeHistory' . view siExchange

---
--- Order
---

instance (Exchange e, MonadIO (ExchangeM e), MonadError String (ExchangeM e), OrderP e, OrderT e ~ Order, OrderAmountT e ~ Double, OrderTypeT e ~ OrderType, OrderResponseT e ~ OrderResponse) => OrderP (Sim e) where
    type OrderTypeT (Sim e) = OrderType
    type OrderAmountT (Sim e) = Double
    type OrderT (Sim e) = Order
    type OrderResponseT (Sim e) = OrderResponse
    currentOrders' sim = do
        stat <- readState sim
        let plos = view ssPendingLimitOrders stat
        return $ fmap toOrder plos
    placeLimitOrder' sim o = do
        case (view oType o) of
            BID -> guardOrderBalance BID (view oVolume o * view oUnitPrice o) sim
            ASK -> guardOrderBalance ASK (view oVolume o) sim
        nid <- operateState newID sim
        let plo = toPLO $ set oID nid o
        modifyState (over ssPendingLimitOrders (++[plo])) sim
        return $ OrderResponse nid
    placeMarketOrder' sim typ amount = do
        guardOrderBalance typ amount sim
        nid <- operateState newID sim
        let pmo = PendingMarketOrder typ nid amount
        modifyState (over ssPendingMarketOrders (++[pmo])) sim
        return $ OrderResponse nid
    cancelOrder' sim o = modifyState (cancelO $ view oID o) sim

toOrder :: PendingLimitOrder -> Order
toOrder plo =
    Order {
        _oType = view ploType plo,
        _oID = view ploID plo,
        _oTimeUTC = fmap (flip div 1000000) $ view ploTimeUTCUS plo,
        _oExpirationTimeUTC = fmap (flip div 1000000) $ view ploExpirationTimeUTCUS plo,
        _oVolume = view ploVolume plo,
        _oUnitPrice = view ploUnitPrice plo
    }

toPLO :: Order -> PendingLimitOrder
toPLO o =
    PendingLimitOrder {
        _ploType = view oType o,
        _ploID = view oID o,
        _ploTimeUTCUS = fmap (* 1000000) $ view oTimeUTC o,
        _ploExpirationTimeUTCUS = fmap (* 1000000) $ view oExpirationTimeUTC o,
        _ploVolume = view oVolume o,
        _ploUnitPrice = view oUnitPrice o,
        _ploOutstanding = view oVolume o
    }

guardOrderBalance :: (Exchange e, MonadIO (ExchangeM e), MonadError String (ExchangeM e), OrderP e, OrderTypeT e ~ OrderType, OrderAmountT e ~ Double) => OrderType -> Double -> Sim e -> (ExchangeM e) ()
guardOrderBalance BID amount sim = do
    curBal <- viewState ssCurrencyBalance sim
    when (curBal < amount) $ throwError "Currency balance is too low to place order"
guardOrderBalance ASK amount sim = do
    comBal <- viewState ssCommodityBalance sim
    when (comBal < amount) $ throwError "Commodity balance is too low to place order"

cancelO :: String -> SimState -> SimState
cancelO oid stat = set ssPendingLimitOrders plos stat
    where plos = filter ((/=oid) . view ploID) $ view ssPendingLimitOrders stat

newID :: SimState -> (String, SimState)
newID stat = (show nid, set ssIDGen nid stat)
    where nid = view ssIDGen stat + 1

---
--- Balances
---

instance (Exchange e, MonadIO (ExchangeM e), BalancesP e, BalancesT e ~ Balances) => BalancesP (Sim e) where
    type BalancesT (Sim e) = Balances
    balances' sim = do
        stat <- readState sim
        let cur = view ssCurrencyBalance stat
        let com = view ssCommodityBalance stat
        return $ Balances cur com
