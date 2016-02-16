{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Mimir.OKCoin(
    module Mimir.Types,
    module Mimir.OKCoin.Types
) where

import Mimir.API
import Mimir.Types
import Mimir.HTTP
import Mimir.OKCoin.Types
import Mimir.OKCoin.Instances

import Control.Lens (Lens, view, set, at, _Just)
import Control.Monad.Trans
import Crypto.Hash.MD5 (hash)
import Data.Aeson (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Base16 as B16
import Data.Char (toLower, toUpper)
import Data.List (intersperse, sortOn)
import qualified Data.Map as M
import Data.Monoid
import Numeric (showFFloat)

instance Body String where
    encodeBody = BL.pack

instance HasManager OKCoin where
    getManager = view ocManager

---
--- Ticker
---

instance TickerP OKCoin where
    type TickerT OKCoin = Ticker
    ticker = apiReq "ticker.do" []

---
--- Candles
---

instance CandlesP OKCoin where
    type CandleIntervalT OKCoin = OKCandleInterval
    type CandleT OKCoin = Candle
    candles iv = do
        (PriceHistory cx) <- apiReq "kline.do" [("type", show iv), ("size", "100")]
        return cx

---
--- OrderBook
---

instance OrderBookP OKCoin where
    type OrderBookT OKCoin = OrderBook
    orderBook = apiReq "depth.do" []

---
--- TradeHistory
---

instance TradeHistoryP OKCoin where
    type TradeT OKCoin = Trade
    tradeHistory = apiReq "trades.do" []

---
--- Spot
---

instance SpotP OKCoin where
    type SpotBalancesT OKCoin = Balances
    type SpotOrderTypeT OKCoin = OrderType
    type SpotOrderAmountT OKCoin = Double
    type SpotOrderT OKCoin = Order
    type SpotOrderIDT OKCoin = String
    spotBalances = do
        (OKBalances bx) <- apiReqAuth "userinfo.do" []
        cur <- getBalance ocCurrencySymbol bx
        com <- getBalance ocCommoditySymbol bx
        return $ Balances cur com
    currentSpotOrders = do
        sym <- viewTradeM ocSymbol
        (Orders ox) <- apiReqAuth "order_history.do" [("symbol", sym), ("status", "0"), ("current_page", "0"), ("page_length", "200")]
        return ox
    placeLimitSpotOrder typ vol price = do
        sym <- viewTradeM ocSymbol
        (OrderResponse oid) <- apiReqAuth "trade.do" [("symbol", sym), ("type", otyp $ typ), ("price", encNum $ price), ("amount", encNum $ vol)]
        return oid
        where
        otyp BID = "buy"
        otyp ASK = "sell"
    placeMarketSpotOrder BID amount = do
        sym <- viewTradeM ocSymbol
        (OrderResponse oid) <- apiReqAuth "trade.do" [("symbol", sym), ("type", "buy_market"), ("price", encNum amount)]
        return oid
    placeMarketSpotOrder ASK amount = do
        sym <- viewTradeM ocSymbol
        (OrderResponse oid) <- apiReqAuth "trade.do" [("symbol", sym), ("type", "buy_market"), ("amount", encNum amount)]
        return oid
    cancelSpotOrder oid = do
        sym <- viewTradeM ocSymbol
        apiReqAuth "cancel_order.do" [("symbol", sym), ("order_id", oid)]

getBalance :: Lens OKCoin OKCoin String String -> [(String, Double)] -> TradeM OKCoin Double
getBalance lens bx = do
    csym <- viewTradeM lens
    let mv = M.lookup csym $ M.fromList bx
    maybe (return 0.0) (return) mv

---
--- Utility
---

apiReq :: FromJSON r => String -> [(String, String)] -> TradeM OKCoin r
apiReq endpoint params = do
    baseURL <- viewTradeM ocBaseURL
    sym <- viewTradeM ocSymbol
    key <- viewTradeM ocApiKey
    let qstr = mconcat . intersperse "&" . fmap toParam . sortOn fst . M.toList . set (at "api_key") (Just key) . set (at "symbol") (Just sym) . M.fromList $ params
    let url = baseURL ++ endpoint ++ "?" ++ qstr
    req <- buildReq url "GET" [] noBody
    httpJSON req

apiReqAuth :: FromJSON r => String -> [(String, String)] -> TradeM OKCoin r
apiReqAuth endpoint params = do
    baseURL <- viewTradeM ocBaseURL
    key <- viewTradeM ocApiKey
    let px = sortOn fst . M.toList . set (at "api_key") (Just key) . M.fromList $ params
    sig <- reqSig px
    let body = mconcat . intersperse "&" . fmap toParam $ px ++ [("sign", sig)]
    req <- buildReq (baseURL ++ endpoint) "POST" [("Content-Type", "application/x-www-form-urlencoded")] (Just body)
    httpJSON req

reqSig :: [(String, String)] -> TradeM OKCoin String
reqSig px = do
    sym <- viewTradeM ocSymbol
    sec <- viewTradeM ocApiSecret
    let qstr = mconcat . intersperse "&" . fmap toParam . sortOn fst $ px
    let sig = fmap toUpper . B.unpack . B16.encode . hash . B.pack $ qstr ++ "&secret_key=" ++ sec
    return sig

toParam :: (String, String) -> String
toParam (k, v) = k ++ "=" ++ v

toFormParam :: (String, String) -> (B.ByteString, B.ByteString)
toFormParam (k, v) = (B.pack k, B.pack v)

noBody :: Maybe String
noBody = Nothing

encNum :: Double -> String
encNum n = showFFloat (Just 6) n ""
