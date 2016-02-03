{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Mimir.Sim(
    module Mimir.Std,
    module Mimir.Sim.Types
) where

import Mimir.Types
import Mimir.Std
import Mimir.Sim.Types

import Control.Lens (view)

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
