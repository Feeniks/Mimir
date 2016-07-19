
module Mimir.API(
    log_,
    err,
    ticker,
    spotBalances,
    currentSpotOrders,
    placeSpotOrder,
    cancelSpotOrder
) where

import Mimir.Types
import Mimir.Free

import Data.Proxy

log_ :: String -> Free (Cmd e) ()
log_ = liftF . LogC

err :: String -> Free (Cmd e) a
err = liftF . ErrC

--
-- TickerP
--

ticker :: (Exchange e, TickerP e) => Free (Cmd e) (TickerT e)
ticker = liftF $ ExC ticker'

--
-- Spot
--

spotBalances :: (Exchange e, SpotP e) => Free (Cmd e) (SpotBalancesT e)
spotBalances = liftF $ ExC spotBalances'

currentSpotOrders :: (Exchange e, SpotP e) => Free (Cmd e) [SpotOrderT e]
currentSpotOrders = liftF $ ExC currentSpotOrders'

placeSpotOrder :: (Exchange e, SpotP e) => SpotOrderT e -> Free (Cmd e) (SpotOrderIDT e)
placeSpotOrder = liftF . ExC . placeSpotOrder'

cancelSpotOrder  :: (Exchange e, SpotP e) => SpotOrderIDT e -> Free (Cmd e) ()
cancelSpotOrder = liftF . ExC . cancelSpotOrder'
