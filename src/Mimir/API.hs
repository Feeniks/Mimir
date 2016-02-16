{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Mimir.API where

import Mimir.Types

import Control.Eff
import Control.Eff.Exception
import Control.Eff.Lift
import Control.Eff.Operational
import Control.Eff.Reader.Strict
import Control.Lens (Lens, view)
import Data.Proxy
import Data.Typeable

---
--- Utility
---

viewE :: (Member (Reader e) r, Typeable e) => Lens e e a a -> Eff r a
viewE lens = ask >>= return . view lens

---
--- Ops
---

ticker :: Member (Program MarketData) r => Eff r Ticker
ticker = singleton TickerF

orderBook :: Member (Program MarketData) r => Eff r OrderBook
orderBook = singleton OrderBookF

---
--- MarketData
---

data MarketData a where
    TickerF :: MarketData Ticker
    OrderBookF :: MarketData OrderBook

class MarketDataP e where
    runMarketData :: (Member (Lift IO) r, SetMember Lift (Lift IO) r, Member (Reader e) r, Member (Exc String) r) => Proxy e -> Eff (Program MarketData :> r) a -> Eff r a
