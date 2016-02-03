{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Mimir.Test(
    module Mimir.Std,
    module Mimir.Test.Types
) where

import Mimir.Types
import Mimir.Std
import Mimir.Test.Types

import Control.Lens (view)

instance HasStd Test where
    getStd = view teExchange

instance Exchange e => Exchange (Test e) where
    type ExchangeM (Test e) = ExchangeM e

---
--- Ticker
---

instance (Exchange e, TickerP e) => TickerP (Test e) where
    type TickerT (Test e) = TickerT e
    ticker' = ticker' . view teExchange

---
--- Candles
---

instance (Exchange e, CandlesP e) => CandlesP (Test e) where
    type CandleIntervalT (Test e) = CandleIntervalT e
    type CandleT (Test e) = CandleT e
    candles' t iv = candles' (view teExchange t) iv

---
--- OrderBook
---

instance (Exchange e, OrderBookP e) => OrderBookP (Test e) where
    type OrderBookT (Test e) = OrderBookT e
    orderBook' = orderBook' . view teExchange

---
--- TradeHistory
---

instance (Exchange e, TradeHistoryP e) => TradeHistoryP (Test e) where
    type TradeT (Test e) = TradeT e
    tradeHistory' = tradeHistory' . view teExchange
