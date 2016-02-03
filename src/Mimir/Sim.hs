{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Mimir.Sim(
    module Mimir.Std,
    Sim
) where

import Mimir.Types
import Mimir.Std
import Mimir.Sim.Types

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Lens (over, set, view)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Trans
import Data.Time.Clock.POSIX (getPOSIXTime)

readState :: (Exchange e, MonadIO (ExchangeM e)) => Sim e -> (ExchangeM e) SimState
readState = liftIO . atomically . readTVar . view siState

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
        nid <- operateState newID sim
        let plo = toPLO $ set oID nid o
        modifyState (over ssPendingLimitOrders (++[plo])) sim
        return $ OrderResponse nid
    placeMarketOrder' _ typ amount = undefined
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
