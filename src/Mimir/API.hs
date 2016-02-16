{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Mimir.API where

import Control.Lens (Lens, view)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Network.HTTP.Conduit (Manager)

runTradeM :: TradeM e a -> e -> IO (Either TradeErr a)
runTradeM act e = runEitherT $ runReaderT act e

viewTradeM :: Lens e e a a -> TradeM e a
viewTradeM lens = ask >>= return . view lens

---
--- Defs
---

type TradeM s = ReaderT s (EitherT TradeErr IO)
data TradeErr = TradeErr String deriving (Eq, Show)

class HasManager e where
    getManager :: e -> Manager

class TickerP e where
    type TickerT e :: *
    ticker :: TradeM e (TickerT e)

class CandlesP e where
    type CandleIntervalT e :: *
    type CandleT e :: *
    candles :: (CandleIntervalT e) -> TradeM e [CandleT e]

class OrderBookP e where
    type OrderBookT e :: *
    orderBook :: TradeM e (OrderBookT e)

class TradeHistoryP e where
    type TradeT e :: *
    tradeHistory :: TradeM e [TradeT e]

class SpotP e where
    type SpotBalancesT e :: *
    type SpotOrderTypeT e :: *
    type SpotOrderAmountT e :: *
    type SpotOrderT e :: *
    type SpotOrderIDT e :: *
    spotBalances :: TradeM e (SpotBalancesT e)
    currentSpotOrders :: TradeM e [SpotOrderT e]
    placeLimitSpotOrder :: SpotOrderTypeT e -> SpotOrderAmountT e -> SpotOrderAmountT e -> TradeM e (SpotOrderIDT e)
    placeMarketSpotOrder :: SpotOrderTypeT e -> SpotOrderAmountT e -> TradeM e (SpotOrderIDT e)
    cancelSpotOrder :: SpotOrderIDT e -> TradeM e ()

class Iso a b where
    isoF :: a -> b
    isoG :: b -> a

instance Iso a a where
    isoF = id
    isoG = id
