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

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Lens (Lens, over, set, view, (&), (.~), (%~))
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either (left)
import Data.Maybe (fromJust, isJust, listToMaybe)
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
        _ssCurrencyReserved = 0,
        _ssCommodityReserved = 0,
        _ssPendingLimitOrders = [],
        _ssPendingMarketOrders = []
    }
    tstat <- atomically $ newTVar stat
    tid <- forkIO $ runSim cycleDelayMS tstat e
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

runSim :: Exchange e => Int -> TVar SimState -> e -> IO ()
runSim cycleDelayMS tstat e = forever $ do
    --TODO: impl
    threadDelay $ cycleDelayMS * 1000

---
--- Utility
---

runExchange :: (Exchange e, Iso StdErr (ErrorT e)) => (e -> ExchangeM e a) -> Sim e -> StdM (Sim e) a
runExchange f sim = do
    let ex = view siExchange sim
    res <- liftIO $ reifyIO (f ex) ex
    case res of
        Left e -> lift . left $ isoG e
        Right r -> return r

readState :: Exchange e => Sim e -> StdM (Sim e) SimState
readState = liftIO . atomically . readTVar . view siState

viewState :: Exchange e => Lens SimState SimState r r -> Sim e -> StdM (Sim e) r
viewState lens = liftIO . atomically . fmap (view lens) . readTVar . view siState

operateState :: Exchange e => (SimState -> (a, SimState)) -> Sim e -> StdM (Sim e) a
operateState f sim = liftIO . atomically $ do
    let tv = view siState sim
    (res, stat) <- fmap f $ readTVar tv
    writeTVar tv stat
    return res

modifyState :: Exchange e => (SimState -> SimState) -> Sim e -> StdM (Sim e) ()
modifyState f = operateState $ (\s -> ((), f s))

---
--- Exchange instance
---

instance Exchange e => Exchange (Sim e) where
    type ExchangeM (Sim e) = StdM (Sim e)
    type ErrorT (Sim e) = StdErr
    reifyIO = reifyStdM

---
--- Ticker
---

instance (Exchange e, Monad (ExchangeM e), Iso StdErr (ErrorT e), TickerP e) => TickerP (Sim e) where
    type TickerT (Sim e) = TickerT e
    ticker' = runExchange ticker'

---
--- Candles
---

instance (Exchange e, Monad (ExchangeM e), Iso StdErr (ErrorT e), CandlesP e) => CandlesP (Sim e) where
    type CandleIntervalT (Sim e) = CandleIntervalT e
    type CandleT (Sim e) = CandleT e
    candles' t iv = runExchange (\e -> candles' e iv) t

---
--- OrderBook
---

instance (Exchange e, Monad (ExchangeM e), Iso StdErr (ErrorT e), OrderBookP e) => OrderBookP (Sim e) where
    type OrderBookT (Sim e) = OrderBookT e
    orderBook' = runExchange orderBook'

---
--- TradeHistory
---

instance (Exchange e, Monad (ExchangeM e), Iso StdErr (ErrorT e), TradeHistoryP e) => TradeHistoryP (Sim e) where
    type TradeT (Sim e) = TradeT e
    tradeHistory' = runExchange tradeHistory'


---
--- Order
---

instance (Exchange e, Monad (ExchangeM e), OrderP e, Iso Order (OrderT e), Iso OrderType (OrderTypeT e), Iso Double (OrderAmountT e), Iso OrderResponse (OrderResponseT e)) => OrderP (Sim e) where
    type OrderTypeT (Sim e) = OrderTypeT e
    type OrderAmountT (Sim e) = OrderAmountT e
    type OrderT (Sim e) = OrderT e
    type OrderResponseT (Sim e) = OrderResponseT e
    currentOrders' sim = do
        stat <- readState sim
        let plos = view ssPendingLimitOrders stat
        return . fmap isoF $ fmap toOrder plos
    placeLimitOrder' sim eo = do
        let o = isoG eo
        nid <- operateState newID sim
        let plo = toPLO $ set oID nid o
        ok <- operateState (addPLO plo) sim
        case ok of
            True ->  return . isoF $ OrderResponse nid
            False -> lift . left $ StdErr "Insufficient balance for this trade"
    placeMarketOrder' sim etyp eamount = do
        let typ = isoG etyp
        let amount = isoG eamount
        nid <- operateState newID sim
        let pmo = PendingMarketOrder typ nid amount
        ok <- operateState (addPMO pmo) sim
        case ok of
            True ->  return . isoF $ OrderResponse nid
            False -> lift . left $ StdErr "Insufficient balance for this trade"
    cancelOrder' sim eo = modifyState (cancelO . view oID $ isoG eo) sim

addPLO :: PendingLimitOrder -> SimState -> (Bool, SimState)
addPLO plo stat
    | rem > 0 = (True, stat & ssPendingLimitOrders %~ (++[plo]) & rlens %~ (+ amount))
    | otherwise = (False, stat)
    where
    typ = view ploType plo
    rlens = case typ of
        BID -> ssCurrencyReserved
        ASK -> ssCommodityReserved
    amount = case typ of
        BID -> (_ploVolume plo) * (_ploUnitPrice plo)
        ASK -> view ploVolume plo
    rem = case typ of
        BID -> (_ssCurrencyBalance stat) - amount
        ASK -> (_ssCommodityBalance stat) - amount

addPMO :: PendingMarketOrder -> SimState -> (Bool, SimState)
addPMO pmo stat
    | rem > 0 = (True, stat & ssPendingMarketOrders %~ (++[pmo]) & rlens %~ (+ amount))
    | otherwise = (False, stat)
    where
    typ = view pmoType pmo
    rlens = case typ of
        BID -> ssCurrencyReserved
        ASK -> ssCommodityReserved
    amount = view pmoAmount pmo
    rem = case typ of
        BID -> (_ssCurrencyBalance stat) - amount
        ASK -> (_ssCommodityBalance stat) - amount

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

cancelO :: String -> SimState -> SimState
cancelO oid stat
    | orderExists = stat & ssPendingLimitOrders %~ (filter ((/=oid) . view ploID)) & rlens %~ (\b -> b - amount)
    | otherwise = stat
    where
    plo = listToMaybe . filter ((==oid) . view ploID) $ view ssPendingLimitOrders stat
    orderExists = isJust plo
    rlens = case (fmap (view ploType) plo) of
        Nothing -> undefined
        (Just BID) -> ssCurrencyReserved
        (Just ASK) -> ssCommodityReserved
    amount = case (fmap (view ploType) plo) of
        Nothing -> 0.0
        (Just BID) -> (fromJust $ fmap _ploVolume plo) * (fromJust $ fmap _ploUnitPrice plo)
        (Just ASK) -> (fromJust $ fmap _ploVolume plo)

newID :: SimState -> (String, SimState)
newID stat = (show nid, set ssIDGen nid stat)
    where nid = view ssIDGen stat + 1

---
--- Balances
---

instance (Exchange e, Monad (ExchangeM e), BalancesP e, Iso Balances (BalancesT e)) => BalancesP (Sim e) where
    type BalancesT (Sim e) = BalancesT e
    balances' sim = do
        stat <- readState sim
        let cur = view ssCurrencyBalance stat
        let com = view ssCommodityBalance stat
        let curRes = view ssCurrencyReserved stat
        let comRes = view ssCommodityReserved stat
        return . isoF $ Balances (cur - curRes) (com - comRes)
