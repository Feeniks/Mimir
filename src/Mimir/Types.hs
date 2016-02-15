{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Mimir.Types where

class Exchange e where
    type ExchangeM e :: * -> *
    type ErrorT e :: *
    reifyIO :: (ExchangeM e) a -> e -> IO (Either (ErrorT e) a)

class TickerP e where
    type TickerT e :: *
    ticker' :: (Exchange e, Monad (ExchangeM e)) => e -> (ExchangeM e) (TickerT e)

class CandlesP e where
    type CandleIntervalT e :: *
    type CandleT e :: *
    candles' :: (Exchange e, Monad (ExchangeM e)) => e -> (CandleIntervalT e) -> (ExchangeM e) [CandleT e]

class OrderBookP e where
    type OrderBookT e :: *
    orderBook' :: (Exchange e, Monad (ExchangeM e)) => e -> (ExchangeM e) (OrderBookT e)

class TradeHistoryP e where
    type TradeT e :: *
    tradeHistory' :: (Exchange e, Monad (ExchangeM e)) => e -> (ExchangeM e) [TradeT e]

class SpotP e where
    type SpotBalancesT e :: *
    type SpotOrderTypeT e :: *
    type SpotOrderAmountT e :: *
    type SpotOrderT e :: *
    type SpotOrderIDT e :: *
    spotBalances' :: (Exchange e, Monad (ExchangeM e)) => e -> (ExchangeM e) (SpotBalancesT e)
    currentSpotOrders' :: (Exchange e, Monad (ExchangeM e)) => e -> (ExchangeM e) [SpotOrderT e]
    placeLimitSpotOrder' :: (Exchange e, Monad (ExchangeM e)) => e -> SpotOrderTypeT e -> SpotOrderAmountT e -> SpotOrderAmountT e -> (ExchangeM e) (SpotOrderIDT e)
    placeMarketSpotOrder' :: (Exchange e, Monad (ExchangeM e)) => e -> SpotOrderTypeT e -> SpotOrderAmountT e -> (ExchangeM e) (SpotOrderIDT e)
    cancelSpotOrder' :: (Exchange e, Monad (ExchangeM e)) => e -> SpotOrderIDT e -> (ExchangeM e) ()

class Iso a b where
    isoF :: a -> b
    isoG :: b -> a

instance Iso a a where
    isoF = id
    isoG = id
