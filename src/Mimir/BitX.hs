{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Mimir.BitX(
    module Mimir.Types,
    module Mimir.BitX.Types
) where

import Mimir.API
import Mimir.Types
import Mimir.HTTP
import Mimir.BitX.Types
import Mimir.BitX.Instances

import Control.Lens (view)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson (ToJSON, encode)
import Data.Monoid
import Network.HTTP.Conduit (urlEncodedBody)
import Numeric (showFFloat)

instance Body BL.ByteString where
    encodeBody = id

instance HasManager BitX where
    getManager = view bxManager

---
--- Ticker
---

instance TickerP BitX where
    type TickerT BitX = Ticker
    ticker = do
        url <- marketURL "ticker"
        req <- buildReq url "GET" [] noBody
        httpJSON req

---
--- Candles
---

instance CandlesP BitX where
    type CandleIntervalT BitX = CandleInterval
    type CandleT BitX = Candle
    candles iv = do
        pairCode <- viewTradeM bxPairCode
        let url = "https://bitx.co/ajax/1/candles?pair=" <> pairCode <> "&duration=" <> (show iv)
        req <- buildReq url "GET" [] noBody
        (PriceHistory cx) <- httpJSON req
        return cx

---
--- OrderBook
---

instance OrderBookP BitX where
    type OrderBookT BitX = OrderBook
    orderBook = do
        url <- marketURL "orderbook"
        req <- buildReq url "GET" [] noBody
        httpJSON req

---
--- TradeHistory
---

instance TradeHistoryP BitX where
    type TradeT BitX = Trade
    tradeHistory = do
        url <- marketURL "trades"
        req <- buildReq url "GET" [] noBody
        (TradeHistory tx) <- httpJSON req
        return tx

---
--- Spot
---

instance SpotP BitX where
    type SpotBalancesT BitX = Balances
    type SpotOrderTypeT BitX = OrderType
    type SpotOrderAmountT BitX = Double
    type SpotOrderT BitX = Order
    type SpotOrderIDT BitX = String
    spotBalances = do
        url <- marketURL "balance"
        req <- buildReq url "GET" [] noBody
        (Accounts ax) <- httpJSON req
        toBalances ax
    currentSpotOrders = do
        bURL <- marketURL "listorders"
        req <- buildReq (bURL <> "&state=PENDING") "GET" [] noBody
        (Orders mox) <- httpJSON req
        maybe (return []) return mox
    placeLimitSpotOrder typ vol price = do
        bURL <- viewTradeM bxBaseURL
        params <- mkLimitOrder typ vol price
        req <- fmap (urlEncodedBody params) $ buildReq (bURL <> "postorder") "POST" [] noBody
        (OrderResponse oid) <- httpJSON req
        return oid
    placeMarketSpotOrder typ amount = do
        bURL <- viewTradeM bxBaseURL
        params <- mkMarketOrder typ amount
        req <- fmap (urlEncodedBody params) $ buildReq (bURL <> "marketorder") "POST" [] noBody
        (OrderResponse oid) <- httpJSON req
        return oid
    cancelSpotOrder oid = do
        bURL <- viewTradeM bxBaseURL
        req <- fmap (urlEncodedBody [("order_id", B.pack $ oid)]) $ buildReq (bURL <> "stoporder") "POST" [] noBody
        http' req

---
--- Utility
---

toBalances :: [Account] -> TradeM BitX Balances
toBalances ax = do
    currencyCode <- viewTradeM bxCurrencyCode
    commodityCode <- viewTradeM bxCommodityCode
    let currency = sum . fmap (\a -> _acBalance a - _acReserved a) . filter ((==currencyCode) . view acAssetCode) $ ax
    let commodity = sum . fmap (\a -> _acBalance a - _acReserved a) . filter ((==commodityCode) . view acAssetCode) $ ax
    return $ Balances currency commodity

marketURL :: String -> TradeM BitX String
marketURL endpoint = do
    baseURL <- viewTradeM bxBaseURL
    pairCode <- viewTradeM bxPairCode
    let url = baseURL <> endpoint <> "?pair=" <> pairCode
    return url

mkLimitOrder :: OrderType -> Double -> Double -> TradeM BitX [(B.ByteString, B.ByteString)]
mkLimitOrder typ vol price = do
    pairCode <- viewTradeM bxPairCode
    return [("pair", B.pack pairCode), ("type", B.pack $ show typ), ("volume", encNum $ vol), ("price", encNum $ price)]

mkMarketOrder :: OrderType -> Double -> TradeM BitX [(B.ByteString, B.ByteString)]
mkMarketOrder typ amount = do
    pairCode <- viewTradeM bxPairCode
    case typ of
        BID -> return [("pair", B.pack pairCode), ("type", "BUY"), ("counter_volume", encNum amount)]
        ASK -> return [("pair", B.pack pairCode), ("type", "SELL"), ("base_volume", encNum amount)]

encNum :: Double -> B.ByteString
encNum n = B.pack $ showFFloat (Just 6) n ""

noBody :: Maybe BL.ByteString
noBody = Nothing
