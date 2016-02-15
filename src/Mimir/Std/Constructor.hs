{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Mimir.Std.Constructor(
    Std(..)
) where

import Mimir.Types
import Mimir.Std.Types

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either

data Std e = Std e

runExchange :: (Exchange e, Monad (ExchangeM e), Iso StdErr (ErrorT e)) => (e -> (ExchangeM e) a) -> (Std e) -> StdM e a
runExchange act (Std e) = do
    res <- liftIO $ reifyIO (act e) e
    case res of
        Left e -> lift . left $ isoG e
        Right r -> return r

instance (Exchange e, ExchangeM e ~ StdM e, Iso StdErr (ErrorT e)) => Exchange (Std e) where
    type ExchangeM (Std e) = StdM e
    type ErrorT (Std e) = StdErr
    reifyIO act (Std e) = do
        res <- liftIO $ reifyIO act e
        case res of
            Left e -> return . Left $ isoG e
            Right r -> return $ Right r

---
--- Ticker
---

instance (Exchange e, Monad (ExchangeM e), Iso StdErr (ErrorT e), TickerP e) => TickerP (Std e) where
    type TickerT (Std e) = TickerT e
    ticker' = runExchange ticker'

---
--- Candles
---

instance (Exchange e, Monad (ExchangeM e), Iso StdErr (ErrorT e), CandlesP e) => CandlesP (Std e) where
    type CandleIntervalT (Std e) = CandleIntervalT e
    type CandleT (Std e) = CandleT e
    candles' s iv = runExchange (\e -> candles' e iv) s

---
--- OrderBook
---

instance (Exchange e, Monad (ExchangeM e), Iso StdErr (ErrorT e), OrderBookP e) => OrderBookP (Std e) where
    type OrderBookT (Std e) = OrderBookT e
    orderBook' = runExchange orderBook'

---
--- TradeHistory
---

instance (Exchange e, Monad (ExchangeM e), Iso StdErr (ErrorT e), TradeHistoryP e) => TradeHistoryP (Std e) where
    type TradeT (Std e) = TradeT e
    tradeHistory' = runExchange tradeHistory'

---
--- Spot
---

instance (Exchange e, Monad (ExchangeM e), Iso StdErr (ErrorT e), SpotP e) => SpotP (Std e) where
    type SpotBalancesT (Std e) = SpotBalancesT e
    type SpotOrderTypeT (Std e) = SpotOrderTypeT e
    type SpotOrderAmountT (Std e) = SpotOrderAmountT e
    type SpotOrderT (Std e) = SpotOrderT e
    type SpotOrderIDT (Std e) = SpotOrderIDT e
    spotBalances' = runExchange spotBalances'
    currentSpotOrders' = runExchange currentSpotOrders'
    placeLimitSpotOrder' s typ vol price = runExchange (\e -> placeLimitSpotOrder' e typ vol price) s
    placeMarketSpotOrder' s typ amount = runExchange (\e -> placeMarketSpotOrder' e typ amount) s
    cancelSpotOrder' s o = runExchange (\e -> cancelSpotOrder' e o) s
