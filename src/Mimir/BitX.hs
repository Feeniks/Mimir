{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Mimir.BitX(
    module Mimir.BitX.Types
) where

import Mimir.API
import Mimir.Types
import Mimir.BitX.Types
import Mimir.BitX.Instances
import Mimir.HTTP

import Control.Eff
import Control.Eff.Exception
import Control.Eff.Lift
import Control.Eff.Operational
import Control.Eff.Reader.Strict
import Data.Aeson (FromJSON)
import Data.ByteString.Lazy.Char8 as BL
import Data.Proxy

instance MarketDataP BitX where
    runMarketData _ = runProgram marketData'

marketData' :: (Member (Lift IO) r, SetMember Lift (Lift IO) r, Member (Reader BitX) r, Member (Exc String) r) => MarketDataAPI a -> Eff r a
marketData' TickerF = publicAPI "ticker"
marketData' OrderBookF = publicAPI "orderbook"

---
--- Utility
---

publicAPI :: (FromJSON b, Member (Lift IO) r, SetMember Lift (Lift IO) r, Member (Reader BitX) r, Member (Exc String) r) => String -> Eff r b
publicAPI endpoint = do
    mgr <- viewE bxManager
    url <- marketURL endpoint
    req <- buildReq url "GET" [] noBody
    httpJSON req mgr

marketURL :: (Member (Reader BitX) r) => String -> Eff r String
marketURL endpoint = do
    baseURL <- viewE bxBaseURL
    pairCode <- viewE bxPairCode
    let url = baseURL ++ endpoint ++ "?pair=" ++ pairCode
    return url

noBody :: Maybe BL.ByteString
noBody = Nothing
