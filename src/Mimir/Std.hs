{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Mimir.Std(
    module Mimir.Std.Types,
    module Mimir.Std.HTTP,
    reifyStdM,
    viewStdM,
    ticker,
    candles,
    orderBook,
    tradeHistory,
    currentOrders,
    placeLimitOrder,
    placeMarketOrder,
    cancelOrder,
    balances
) where

import Mimir.Types
import Mimir.Std.Types
import Mimir.Std.HTTP

import Control.Lens (view)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Data.Proxy

reifyStdM :: (Exchange e, ExchangeM e ~ StdM e) => StdM e a -> e -> IO (Either StdErr a)
reifyStdM act = runEitherT . runReaderT act

ticker :: (Exchange e, ExchangeM e ~ StdM e, TickerP e) => StdM e (TickerT e)
ticker = ticker' =<< ask

candles :: (Exchange e, ExchangeM e ~ StdM e, CandlesP e) => CandleIntervalT e -> StdM e [CandleT e]
candles iv = flip candles' iv =<< ask

orderBook :: (Exchange e, ExchangeM e ~ StdM e, OrderBookP e) => StdM e (OrderBookT e)
orderBook = orderBook' =<< ask

tradeHistory :: (Exchange e, ExchangeM e ~ StdM e, TradeHistoryP e) => StdM e [TradeT e]
tradeHistory = tradeHistory' =<< ask

currentOrders :: (Exchange e, ExchangeM e ~ StdM e, OrderP e) => StdM e [OrderT e]
currentOrders = currentOrders' =<< ask

placeLimitOrder :: (Exchange e, ExchangeM e ~ StdM e, OrderP e) => OrderTypeT e -> OrderAmountT e -> OrderAmountT e -> StdM e (OrderIDT e)
placeLimitOrder typ vol price = ask >>= \e -> placeLimitOrder' e typ vol price

placeMarketOrder :: (Exchange e, ExchangeM e ~ StdM e, OrderP e) => OrderTypeT e -> OrderAmountT e -> StdM e (OrderIDT e)
placeMarketOrder typ vol = ask >>= \e -> placeMarketOrder' e typ vol

cancelOrder :: (Exchange e, ExchangeM e ~ StdM e, OrderP e) => OrderIDT e -> StdM e ()
cancelOrder oid = flip cancelOrder' oid =<< ask

balances :: (Exchange e, ExchangeM e ~ StdM e, BalancesP e) => StdM e (BalancesT e)
balances = balances' =<< ask

viewStdM lens = ask >>= return . view lens
