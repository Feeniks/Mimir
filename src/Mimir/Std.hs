{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Mimir.Std(
    module Mimir.Std.Types,
    module Mimir.Std.HTTP,
    reifyStdM,
    viewStdM,
    throwStd,
    ticker,
    candles,
    orderBook,
    tradeHistory,
    spotBalances,
    currentSpotOrders,
    placeLimitSpotOrder,
    placeMarketSpotOrder,
    cancelSpotOrder
) where

import Mimir.Types
import Mimir.Std.Types
import Mimir.Std.HTTP

import Control.Lens (view)
import Control.Monad
import Control.Monad.Except (throwError)
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Data.Proxy

reifyStdM :: (Exchange e, ExchangeM e ~ StdM e) => StdM e a -> e -> IO (Either StdErr a)
reifyStdM act = runEitherT . runReaderT act

throwStd :: String -> StdM e a
throwStd = throwError . StdErr

ticker :: (Exchange e, ExchangeM e ~ StdM e, TickerP e) => StdM e (TickerT e)
ticker = ticker' =<< ask

candles :: (Exchange e, ExchangeM e ~ StdM e, CandlesP e) => CandleIntervalT e -> StdM e [CandleT e]
candles iv = flip candles' iv =<< ask

orderBook :: (Exchange e, ExchangeM e ~ StdM e, OrderBookP e) => StdM e (OrderBookT e)
orderBook = orderBook' =<< ask

tradeHistory :: (Exchange e, ExchangeM e ~ StdM e, TradeHistoryP e) => StdM e [TradeT e]
tradeHistory = tradeHistory' =<< ask

spotBalances :: (Exchange e, ExchangeM e ~ StdM e, SpotP e) => StdM e (SpotBalancesT e)
spotBalances = spotBalances' =<< ask

currentSpotOrders :: (Exchange e, ExchangeM e ~ StdM e, SpotP e) => StdM e [SpotOrderT e]
currentSpotOrders = currentSpotOrders' =<< ask

placeLimitSpotOrder :: (Exchange e, ExchangeM e ~ StdM e, SpotP e) => SpotOrderTypeT e -> SpotOrderAmountT e -> SpotOrderAmountT e -> StdM e (SpotOrderIDT e)
placeLimitSpotOrder typ vol price = ask >>= \e -> placeLimitSpotOrder' e typ vol price

placeMarketSpotOrder :: (Exchange e, ExchangeM e ~ StdM e, SpotP e) => SpotOrderTypeT e -> SpotOrderAmountT e -> StdM e (SpotOrderIDT e)
placeMarketSpotOrder typ vol = ask >>= \e -> placeMarketSpotOrder' e typ vol

cancelSpotOrder :: (Exchange e, ExchangeM e ~ StdM e, SpotP e) => SpotOrderIDT e -> StdM e ()
cancelSpotOrder oid = flip cancelSpotOrder' oid =<< ask

viewStdM lens = ask >>= return . view lens
