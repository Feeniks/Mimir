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

class OrderP e where
    type OrderTypeT e :: *
    type OrderAmountT e :: *
    type OrderT e :: *
    type OrderIDT e :: *
    currentOrders' :: (Exchange e, Monad (ExchangeM e)) => e -> (ExchangeM e) [OrderT e]
    placeLimitOrder' :: (Exchange e, Monad (ExchangeM e)) => e -> OrderTypeT e -> OrderAmountT e -> OrderAmountT e -> (ExchangeM e) (OrderIDT e)
    placeMarketOrder' :: (Exchange e, Monad (ExchangeM e)) => e -> OrderTypeT e -> OrderAmountT e -> (ExchangeM e) (OrderIDT e)
    cancelOrder' :: (Exchange e, Monad (ExchangeM e)) => e -> OrderIDT e -> (ExchangeM e) ()

class BalancesP e where
    type BalancesT e :: *
    balances' :: (Exchange e, Monad (ExchangeM e)) => e -> (ExchangeM e) (BalancesT e)

class Iso a b where
    isoF :: a -> b
    isoG :: b -> a

instance Iso a a where
    isoF = id
    isoG = id
