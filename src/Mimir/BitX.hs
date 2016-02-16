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
import Control.Lens (view)
import Data.Aeson (FromJSON)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (maybeToList)
import Data.Proxy
import Network.HTTP.Conduit (urlEncodedBody)
import Numeric (showFFloat)

---
--- Market Data
---

instance MarketDataP BitX where
    runMarketData _ = runProgram marketData'

marketData' :: (Member (Lift IO) r, SetMember Lift (Lift IO) r, Member (Reader BitX) r, Member (Exc String) r) => MarketData a -> Eff r a
marketData' TickerF = api "GET" "ticker" [] []
marketData' OrderBookF = api "GET" "orderbook" [] []
marketData' TradeHistoryF = fmap thTrades (api "GET" "trades" [] [])

---
--- Spot Trading
---

instance SpotP BitX where
    runSpot _ = runProgram spot'

spot' :: (Member (Lift IO) r, SetMember Lift (Lift IO) r, Member (Reader BitX) r, Member (Exc String) r) => Spot a -> Eff r a
spot' SpotBalancesF = api "GET" "balance" [] [] >>= toBalances . acsAccounts
spot' CurrentSpotOrdersF = fmap (concat . maybeToList . orsOrders) $ api "GET" "listorders" [("state", "PENDING")] []
spot' (PlaceSpotLimitOrderF t v p) = mkLimitOrder t v p >>= api "POST" "postorder" [] >>= return . orID
spot' (PlaceSpotMarketOrderF t a) = mkMarketOrder t a >>= api "POST" "marketorder" [] >>= return . orID
spot' (CancelSpotOrderF oid) = api "POST" "stoporder" [] [("order_id", B.pack oid)]

toBalances :: Member (Reader BitX) r => [Account] -> Eff r Balances
toBalances ax = do
    currencyCode <- viewE bxCurrencyCode
    commodityCode <- viewE bxCommodityCode
    let currency = sum . fmap (\a -> _acBalance a - _acReserved a) . filter ((==currencyCode) . view acAssetCode) $ ax
    let commodity = sum . fmap (\a -> _acBalance a - _acReserved a) . filter ((==commodityCode) . view acAssetCode) $ ax
    return $ Balances currency commodity

---
--- Utility
---

api :: (FromJSON b, Member (Lift IO) r, SetMember Lift (Lift IO) r, Member (Reader BitX) r, Member (Exc String) r) => B.ByteString -> String -> [(String,String)] -> [(B.ByteString, B.ByteString)] -> Eff r b
api mthd endpoint qparams body = do
    let qs = foldl (\s (k,v) -> s ++ "&" ++ k ++ "=" ++ v) "" qparams
    mgr <- viewE bxManager
    url <- fmap (++qs) $ marketURL endpoint
    req <- case body of
        [] -> buildReq url mthd [] noBody
        _ -> fmap (urlEncodedBody body) $ buildReq url mthd [] noBody
    httpJSON req mgr


marketURL :: (Member (Reader BitX) r) => String -> Eff r String
marketURL endpoint = do
    baseURL <- viewE bxBaseURL
    pairCode <- viewE bxPairCode
    let url = baseURL ++ endpoint ++ "?pair=" ++ pairCode
    return url

mkLimitOrder :: (Member (Reader BitX) r) => OrderType -> Double -> Double -> Eff r [(B.ByteString, B.ByteString)]
mkLimitOrder typ vol price = do
    pairCode <- viewE bxPairCode
    return [("pair", B.pack pairCode), ("type", B.pack $ show typ), ("volume", encNum $ vol), ("price", encNum $ price)]

mkMarketOrder ::(Member (Reader BitX) r) =>  OrderType -> Double -> Eff r [(B.ByteString, B.ByteString)]
mkMarketOrder typ amount = do
    pairCode <- viewE bxPairCode
    case typ of
        BID -> return [("pair", B.pack pairCode), ("type", "BUY"), ("counter_volume", encNum amount)]
        ASK -> return [("pair", B.pack pairCode), ("type", "SELL"), ("base_volume", encNum amount)]

encNum :: Double -> B.ByteString
encNum n = B.pack $ showFFloat (Just 6) n ""

noBody :: Maybe BL.ByteString
noBody = Nothing
