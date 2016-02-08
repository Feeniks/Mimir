{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Mimir.OKCoin(
    module Mimir.Std,
    module Mimir.OKCoin.Types
) where

import Mimir.Types
import Mimir.Std
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

instance Exchange OKCoin where
    type ExchangeM OKCoin = StdM OKCoin
    type ErrorT OKCoin = StdErr
    reifyIO = reifyStdM

---
--- Ticker
---

instance TickerP OKCoin where
    type TickerT OKCoin = Ticker
    ticker' _ = apiReq "ticker.do" []

---
--- Candles
---

instance CandlesP OKCoin where
    type CandleIntervalT OKCoin = OKCandleInterval
    type CandleT OKCoin = Candle
    candles' _ iv = do
        (PriceHistory cx) <- apiReq "kline.do" [("type", show iv), ("size", "100")]
        return cx

---
--- OrderBook
---

instance OrderBookP OKCoin where
    type OrderBookT OKCoin = OrderBook
    orderBook' _ = apiReq "depth.do" []

---
--- TradeHistory
---

instance TradeHistoryP OKCoin where
    type TradeT OKCoin = Trade
    tradeHistory' _ = apiReq "trades.do" []

---
--- Balances
---

instance BalancesP OKCoin where
    type BalancesT OKCoin = Balances
    balances' _ = do
        (OKBalances bx) <- apiReqAuth "userinfo.do" []
        cur <- getBalance ocCurrencySymbol bx
        com <- getBalance ocCommoditySymbol bx
        return $ Balances cur com

getBalance :: Lens OKCoin OKCoin String String -> [(String, Double)] -> StdM OKCoin Double
getBalance lens bx = do
    csym <- viewStdM lens
    let mv = M.lookup csym $ M.fromList bx
    maybe (return 0.0) (return) mv

---
--- Order
---

instance OrderP OKCoin where
    type OrderTypeT OKCoin = OrderType
    type OrderAmountT OKCoin = Double
    type OrderT OKCoin = Order
    type OrderIDT OKCoin = String
    currentOrders' _ = do
        sym <- viewStdM ocSymbol
        (Orders ox) <- apiReqAuth "order_history.do" [("symbol", sym), ("status", "0"), ("current_page", "0"), ("page_length", "200")]
        return ox
    placeLimitOrder' _ typ vol price = do
        sym <- viewStdM ocSymbol
        (OrderResponse oid) <- apiReqAuth "trade.do" [("symbol", sym), ("type", otyp $ typ), ("price", encNum $ price), ("amount", encNum $ vol)]
        return oid
        where
        otyp BID = "buy"
        otyp ASK = "sell"
    placeMarketOrder' _ BID amount = do
        sym <- viewStdM ocSymbol
        (OrderResponse oid) <- apiReqAuth "trade.do" [("symbol", sym), ("type", "buy_market"), ("price", encNum amount)]
        return oid
    placeMarketOrder' _ ASK amount = do
        sym <- viewStdM ocSymbol
        (OrderResponse oid) <- apiReqAuth "trade.do" [("symbol", sym), ("type", "buy_market"), ("amount", encNum amount)]
        return oid
    cancelOrder' _ oid = do
        sym <- viewStdM ocSymbol
        apiReqAuth "cancel_order.do" [("symbol", sym), ("order_id", oid)]

---
--- Utility
---

apiReq :: FromJSON r => String -> [(String, String)] -> StdM OKCoin r
apiReq endpoint params = do
    baseURL <- viewStdM ocBaseURL
    sym <- viewStdM ocSymbol
    key <- viewStdM ocApiKey
    let qstr = mconcat . intersperse "&" . fmap toParam . sortOn fst . M.toList . set (at "api_key") (Just key) . set (at "symbol") (Just sym) . M.fromList $ params
    let url = baseURL ++ endpoint ++ "?" ++ qstr
    req <- buildReq url "GET" [] noBody
    httpJSON req

apiReqAuth :: FromJSON r => String -> [(String, String)] -> StdM OKCoin r
apiReqAuth endpoint params = do
    baseURL <- viewStdM ocBaseURL
    key <- viewStdM ocApiKey
    let px = sortOn fst . M.toList . set (at "api_key") (Just key) . M.fromList $ params
    sig <- reqSig px
    let body = mconcat . intersperse "&" . fmap toParam $ px ++ [("sign", sig)]
    req <- buildReq (baseURL ++ endpoint) "POST" [("Content-Type", "application/x-www-form-urlencoded")] (Just body)
    httpJSON req

reqSig :: [(String, String)] -> StdM OKCoin String
reqSig px = do
    sym <- viewStdM ocSymbol
    sec <- viewStdM ocApiSecret
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
