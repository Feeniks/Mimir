{-# LANGUAGE TypeFamilies #-}

module Mimir.Std(
    module Mimir.Std.Types,
    module Mimir.Std.HTTP,
    runStdM,
    viewStdM,
    ticker,
    priceHistory,
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

runStdM :: StdM s a -> s -> IO (Either StdErr a)
runStdM act = runEitherT . runReaderT act

ticker :: (Exchange e, ExchangeM e ~ StdM e, TickerP e) => StdM e (TickerT e)
ticker = ticker' =<< ask

priceHistory :: (Exchange e, ExchangeM e ~ StdM e, PriceHistoryP e) => PriceIntervalT e -> StdM e [PriceSampleT e]
priceHistory iv = flip priceHistory' iv =<< ask

orderBook :: (Exchange e, ExchangeM e ~ StdM e, OrderBookP e) => StdM e (OrderBookT e)
orderBook = orderBook' =<< ask

tradeHistory :: (Exchange e, ExchangeM e ~ StdM e, TradeHistoryP e) => StdM e [TradeT e]
tradeHistory = tradeHistory' =<< ask

currentOrders :: (Exchange e, ExchangeM e ~ StdM e, OrderP e) => StdM e [OrderT e]
currentOrders = currentOrders' =<< ask

placeLimitOrder :: (Exchange e, ExchangeM e ~ StdM e, OrderP e) => OrderT e -> StdM e (OrderResponseT e)
placeLimitOrder o = flip placeLimitOrder' o =<< ask

placeMarketOrder :: (Exchange e, ExchangeM e ~ StdM e, OrderP e) => OrderTypeT e -> Double -> StdM e (OrderResponseT e)
placeMarketOrder typ vol = ask >>= \e -> placeMarketOrder' e typ vol

cancelOrder :: (Exchange e, ExchangeM e ~ StdM e, OrderP e) => OrderT e -> StdM e ()
cancelOrder o = flip cancelOrder' o =<< ask

balances :: (Exchange e, ExchangeM e ~ StdM e, BalancesP e) => StdM e (BalancesT e)
balances = balances' =<< ask

viewStdM lens = ask >>= return . view lens
