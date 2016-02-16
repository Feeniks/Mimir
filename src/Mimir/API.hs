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

tradeHistory :: Member (Program MarketData) r => Eff r [Trade]
tradeHistory = singleton TradeHistoryF

spotBalances :: Member (Program Spot) r => Eff r Balances
spotBalances = singleton SpotBalancesF

currentSpotOrders :: Member (Program Spot) r => Eff r [Order]
currentSpotOrders = singleton CurrentSpotOrdersF

placeSpotLimitOrder :: Member (Program Spot) r => OrderType -> Volume -> UnitPrice -> Eff r String
placeSpotLimitOrder t v p = singleton $ PlaceSpotLimitOrderF t v p

placeSpotMarketOrder :: Member (Program Spot) r => OrderType -> Double -> Eff r String
placeSpotMarketOrder t a = singleton $ PlaceSpotMarketOrderF t a

cancelSpotOrder :: Member (Program Spot) r => String -> Eff r ()
cancelSpotOrder oid = singleton $ CancelSpotOrderF oid

---
--- MarketData
---

data MarketData a where
    TickerF :: MarketData Ticker
    OrderBookF :: MarketData OrderBook
    TradeHistoryF :: MarketData [Trade]

class MarketDataP e where
    runMarketData :: (Member (Lift IO) r, SetMember Lift (Lift IO) r, Member (Reader e) r, Member (Exc String) r) => Proxy e -> Eff (Program MarketData :> r) a -> Eff r a

---
--- Spot Trading
---

data Spot a where
    SpotBalancesF :: Spot Balances
    CurrentSpotOrdersF :: Spot [Order]
    PlaceSpotLimitOrderF :: OrderType -> Volume -> UnitPrice -> Spot String
    PlaceSpotMarketOrderF :: OrderType -> Double -> Spot String
    CancelSpotOrderF :: String -> Spot ()

class SpotP e where
    runSpot :: (Member (Lift IO) r, SetMember Lift (Lift IO) r, Member (Reader e) r, Member (Exc String) r) => Proxy e -> Eff (Program Spot :> r) a -> Eff r a
