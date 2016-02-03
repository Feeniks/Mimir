{-# LANGUAGE TypeFamilies #-}

module Mimir.Std.Func(
    calcMidPrice,
    calcSMA,
    calcEMA
) where

import Mimir.Types
import Mimir.Std

import Control.Lens (view)
import Data.List (sortOn)

calcMidPrice :: (Exchange e, ExchangeM e ~ StdM e, TickerP e, TickerT e ~ Ticker) => StdM e Double
calcMidPrice = do
    t <- ticker
    let ask = view tiAsk t
    let bid = view tiBid t
    return $ (ask + bid) / 2

calcSMA :: (Exchange e, ExchangeM e ~ StdM e, CandlesP e, CandleT e ~ Candle) => CandleIntervalT e -> Int -> StdM e Double
calcSMA iv num = do
    cx <- candles iv
    return $ calc cx
    where calc = (/ fromIntegral num) . sum . take num . fmap (view caClose) . reverse . sortOn (view caTimeUTC)

calcEMA :: (Exchange e, ExchangeM e ~ StdM e, TickerP e, TickerT e ~ Ticker) => Double -> StdM e Double
calcEMA prev = do
    curr <- calcMidPrice
    return $ ((curr - prev) * 0.1818181818) + prev
