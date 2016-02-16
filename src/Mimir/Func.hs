{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Mimir.Func(
    calcMidPrice,
    calcSMA,
    calcEMA
) where

import Mimir.API
import Mimir.Types

import Control.Lens (view)
import Data.List (sortOn)

calcMidPrice :: (TickerP e, Iso Ticker (TickerT e)) => TradeM e Double
calcMidPrice = do
    t <- fmap isoG ticker
    let ask = view tiAsk t
    let bid = view tiBid t
    return $ (ask + bid) / 2

calcSMA :: (CandlesP e, Iso Candle (CandleT e)) => CandleIntervalT e -> Int -> TradeM e Double
calcSMA iv num = do
    cx <- fmap (fmap isoG) $ candles iv
    return $ calc cx
    where calc = (/ fromIntegral num) . sum . take num . fmap (view caClose) . reverse . sortOn (view caTimeUTC)

calcEMA :: (TickerP e, Iso Ticker (TickerT e)) => Double -> TradeM e Double
calcEMA prev = do
    curr <- calcMidPrice
    return $ ((curr - prev) * 0.1818181818) + prev
