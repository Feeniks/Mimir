{-# LANGUAGE OverloadedStrings #-}

module Mimir.OKCoin.Instances where

import Mimir.OKCoin.Types
import Mimir.Std.Types

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Char (toUpper)
import Data.Maybe (catMaybes)
import Data.Vector ((!))
import qualified Data.Vector as V

instance FromJSON Ticker where
    parseJSON (Object v) = do
        tme <- rstr v "date"
        tval <- v .: "ticker"
        ask <- rstr tval "sell"
        bid <- rstr tval "buy"
        last <- rstr tval "last"
        return $ Ticker (tme * 1000) ask bid last
    parseJSON _ = mzero

instance FromJSON PriceHistory where
    parseJSON (Array sx) = PriceHistory <$> (mapM toSample $ V.toList sx)
        where
        toSample (Array vx) = case V.length vx of
            6 -> do
                let (Number timeMS) = vx ! 0
                let (Number open) = vx ! 1
                let (Number high) = vx ! 2
                let (Number low) = vx ! 3
                let (Number close) = vx ! 4
                let (Number volume) = vx ! 5
                return $ PriceSample (truncate $ timeMS / 1000) (realToFrac open) (realToFrac close) (realToFrac high) (realToFrac low) (realToFrac volume)
            _ -> mzero
    parseJSON _ = mzero

instance FromJSON OrderBook where
    parseJSON (Object v) = OrderBook <$> v .: "bids" <*> v .: "asks"
    parseJSON _ = mzero

instance FromJSON OrderBookEntry where
    parseJSON (Array vx) = case V.length vx of
        2 -> do
            let (Number price) = vx ! 0
            let (Number vol) = vx ! 1
            return $ OrderBookEntry (realToFrac vol) (realToFrac price)
        _ -> mzero
    parseJSON _ = mzero

instance FromJSON Orders where
    parseJSON (Object v) = Orders <$> ((v .: "orders") >>= fmap catMaybes . mapM toOrder)
        where
        toOrder (Object vo) = do
            typ <- rtyp vo "type"
            oid <- fmap show (vo .: "order_id" :: Parser Int)
            amt <- vo .: "amount"
            price <- vo .: "price"
            stat <- (vo .: "status" :: Parser Int)
            let order = Order typ oid Nothing Nothing amt price
            case (stat == 0 || stat == 1) of
                True -> return $ Just order
                False -> return Nothing
        rtyp v k = do
            typ <- fmap (fmap toUpper) $ v .: k
            case (typ == ("BUY" :: String)) of
                True -> return BID
                False -> return ASK
    parseJSON _ = mzero

instance FromJSON OrderResponse where
    parseJSON (Object v) = OrderResponse <$> v .: "order_id"
    parseJSON _ = mzero

instance FromJSON Balances where
    parseJSON (Object v) = do
        vinf <- v .: "info"
        vfunds <- vinf .: "funds"
        vfree <- vfunds .: "free"
        curr <- rstr vfree "usd"
        comm <- rstr vfree "btc"
        return $ Balances curr comm
    parseJSON _ = mzero

rstr v k = (v .: k) >>= return . read
