{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module Mimir.Bitfinex(
    module Mimir.Std,
    module Mimir.Bitfinex.Types
) where

import Mimir.Types
import Mimir.Std
import Mimir.Bitfinex.Types
import Mimir.Bitfinex.Instances

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (readTVar, writeTVar)
import Control.Lens
import Control.Monad.Trans (liftIO)
import Crypto.MAC.HMAC (hmac)
import Crypto.Hash.SHA384 (hash)
import Data.Aeson (FromJSON, ToJSON, Value(..), encode, toJSON)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64.Lazy as B64L
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as HMap
import Data.Monoid
import qualified Data.Text as T
import Network.HTTP.Conduit (urlEncodedBody)
import Numeric (showFFloat)

instance Body BL.ByteString where
    encodeBody = id

instance HasManager Bitfinex where
    getManager = view bfManager

instance Exchange Bitfinex where
    type ExchangeM Bitfinex = StdM Bitfinex
    type ErrorT Bitfinex = StdErr
    reifyIO = reifyStdM

---
--- Ticker
---

instance TickerP Bitfinex where
    type TickerT Bitfinex = Ticker
    ticker' _ = do
        sym <- viewStdM bfSymbol
        publicApi $ "pubticker/" ++ sym

---
--- Balances
---

instance BalancesP Bitfinex where
    type BalancesT Bitfinex = Balances
    balances' _ = do
        curSym <- viewStdM bfCurrencySymbol
        comSym <- viewStdM bfCommoditySymbol
        fmap (toBalances curSym comSym) $ authApi "balances" (Nothing :: Maybe ())

toBalances :: String -> String -> [BFBalance] -> Balances
toBalances curSym comSym = uncurry Balances . foldl sumBX (0, 0)
    where
    sumBX (curb, comb) (BFBalance typ sym amt)
        | typ == "exchange" && sym == curSym = (curb + amt, comb)
        | typ == "exchange" && sym == comSym = (curb, comb + amt)
        | otherwise = (curb, comb)

---
--- Utility
---

publicApi :: FromJSON r => String -> StdM Bitfinex r
publicApi path = do
    url <- fmap (++path) $ viewStdM bfBaseURL
    req <- buildReq url "GET" [] noBody
    httpJSON req

authApi :: (ToJSON b, FromJSON r) => String -> Maybe b -> StdM Bitfinex r
authApi path mb = do
    url <- fmap (++path) $ viewStdM bfBaseURL
    payload <- mkPayload path mb
    key <- fmap B.pack $ viewStdM bfApiKey
    secret <- fmap B.pack $ viewStdM bfApiSecret
    let sig = B16.encode . hmac hash 128 secret $ BL.toStrict payload
    req <- buildReq url "POST" [("X-BFX-APIKEY", key), ("X-BFX-PAYLOAD", BL.toStrict payload), ("X-BFX-SIGNATURE", sig)] noBody
    httpJSON req

mkPayload :: ToJSON b => String -> Maybe b -> StdM Bitfinex BL.ByteString
mkPayload p Nothing = mkAuthPayload p $ HMap.empty
mkPayload p (Just b) = getRequestData b >>= mkAuthPayload p

mkAuthPayload :: String -> HMap.HashMap T.Text Value -> StdM Bitfinex BL.ByteString
mkAuthPayload p m = do
    nonce <- newNonce
    ver <- viewStdM bfVersionCode
    let m' = m & (at "nonce") .~ (Just . String . T.pack . show $ nonce) & (at "request") .~ (Just . String . T.pack $ "/" ++ ver ++ "/" ++ p)
    return . B64L.encode . encode $ Object m'

getRequestData :: ToJSON b => b -> StdM Bitfinex (HMap.HashMap T.Text Value)
getRequestData b = do
    case (toJSON b) of
        (Object hm) -> return hm
        _ -> throwStd "Request body must encode to a JSON Object"

newNonce :: StdM Bitfinex Int
newNonce = do
    tv <- viewStdM bfNonce
    liftIO . atomically $ do
        n <- readTVar tv
        let n' = n + 1
        writeTVar tv n'
        return n'

noBody :: Maybe BL.ByteString
noBody = Nothing
