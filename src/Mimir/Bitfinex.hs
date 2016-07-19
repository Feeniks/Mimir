{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Mimir.Bitfinex(
    module Mimir.Bitfinex.Types
) where

import Mimir.Types
import Mimir.Instances
import Mimir.Bitfinex.Types
import Mimir.Bitfinex.Instances

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (readTVar, writeTVar)
import Control.Lens (at, view, (&), (.~))
import Control.Monad.Except (throwError)
import Control.Monad.Trans (liftIO)
import Crypto.Hash (Digest)
import Crypto.Hash.Algorithms (SHA384)
import Crypto.MAC.HMAC (HMAC(..), hmac)
import Data.Aeson (FromJSON, ToJSON(..), Value(..), encode, object, (.=))
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.Text as T
import Network.HTTP.Nano
import Numeric (showFFloat)

instance Exchange Bitfinex where
    type ExchangeM Bitfinex = TradeM Bitfinex

instance TickerP Bitfinex where
    type TickerT Bitfinex = Ticker
    ticker' = publicApi =<< (("pubticker/"<>) <$> view (exchange . bfSymbol))

instance SpotP Bitfinex where
    type SpotBalancesT Bitfinex = Balances
    type SpotOrderT Bitfinex = Order
    type SpotOrderIDT Bitfinex = Int
    spotBalances' = do
        curSym <- view $ exchange . bfCurrencySymbol
        comSym <- view $ exchange . bfCommoditySymbol
        toBalances curSym comSym <$> authApi "balances" (Nothing :: Maybe ())
    currentSpotOrders' = authApi "orders" (Nothing :: Maybe ())
    placeSpotOrder' o = do
        sym <- view (exchange . bfSymbol)
        view oID <$> authApi "order/new" (Just $ encodeOrder sym o)
    cancelSpotOrder' oid = do
        _ <- authApi "order/cancel" . Just $ object ["order_id" .= oid] :: TradeM Bitfinex Value
        return ()

toBalances :: String -> String -> [BFBalance] -> Balances
toBalances curSym comSym = uncurry Balances . foldl sumBX (0, 0)
    where
    sumBX (curb, comb) (BFBalance typ sym amt)
        | typ == "exchange" && sym == curSym = (curb + amt, comb)
        | typ == "exchange" && sym == comSym = (curb, comb + amt)
        | otherwise = (curb, comb)

encodeOrder :: String -> Order -> Value
encodeOrder sym o =
    object [
        "symbol" .= sym,
        "exchange" .= ("bitfinex" :: String),
        "amount" .= showFFloat (Just 6) (_oVolume o) "",
        "price" .= showFFloat (Just 6)  (_oUnitPrice o) "",
        "side" .= orderTypeSide (_oType o),
        "type" .= orderTypeType (_oType o)
    ]

orderTypeSide :: OrderType -> String
orderTypeSide LIMIT_BUY = "buy"
orderTypeSide LIMIT_SELL = "sell"
orderTypeSide MARKET_BUY = "buy"
orderTypeSide MARKET_SELL = "sell"

orderTypeType :: OrderType -> String
orderTypeType LIMIT_BUY = "exchange limit"
orderTypeType LIMIT_SELL = "exchange limit"
orderTypeType MARKET_BUY = "exchange market"
orderTypeType MARKET_SELL = "exchange market"

--
-- Utility
--

publicApi :: FromJSON r => String -> TradeM Bitfinex r
publicApi path = do
    url <- (<>path) <$> view (exchange . bfBaseURL)
    req <- buildReq GET url NoRequestData
    httpJSON $ addHeaders [("Content-Type", "application/json")] req

authApi :: (ToJSON b, FromJSON r) => String -> Maybe b -> TradeM Bitfinex r
authApi path mb = do
    url <- (<>path) <$> view (exchange . bfBaseURL)
    payload <- mkPayload path mb
    key <- view (exchange . bfApiKey)
    secret <- B.pack <$> view (exchange . bfApiSecret)
    let sig = show . hmacGetDigest $ (hmac secret $ BL.toStrict payload :: HMAC SHA384)
    req <- buildReq POST url NoRequestData
    httpJSON $ addHeaders [("X-BFX-APIKEY", key), ("X-BFX-PAYLOAD", BL.unpack payload), ("X-BFX-SIGNATURE", sig)] req

mkPayload :: ToJSON b => String -> Maybe b -> TradeM Bitfinex BL.ByteString
mkPayload p Nothing = mkAuthPayload p $ HM.empty
mkPayload p (Just b) = getRequestData b >>= mkAuthPayload p

mkAuthPayload :: String -> HM.HashMap T.Text Value -> TradeM Bitfinex BL.ByteString
mkAuthPayload p m = do
    nonce <- newNonce
    ver <- view $ exchange . bfVersionCode
    let m' = m & (at "nonce") .~ (Just . String . T.pack . show $ nonce) & (at "request") .~ (Just . String . T.pack $ "/" ++ ver ++ "/" ++ p)
    return . B64.encode . encode $ Object m'

getRequestData :: ToJSON b => b -> TradeM Bitfinex (HM.HashMap T.Text Value)
getRequestData b = do
    case (toJSON b) of
        (Object hm) -> return hm
        _ -> throwError $ TLogicError "Request body must encode to a JSON Object"

newNonce :: TradeM Bitfinex Int
newNonce = do
    tv <- view $ exchange . bfNonce
    liftIO . atomically $ do
        n <- readTVar tv
        let n' = n + 1
        writeTVar tv n'
        return n'
