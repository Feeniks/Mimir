{-# LANGUAGE OverloadedStrings #-}

module Mimir.BitX.Instances() where

import Mimir.Types
import Mimir.BitX.Types

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Char (toUpper)

instance FromJSON Ticker where
    parseJSON (Object v) =
        Ticker  <$> v .: "timestamp"
                <*> rstr v "ask"
                <*> rstr v "bid"
                <*> rstr v "last_trade"
    parseJSON _ = mzero

instance FromJSON OrderBook where
    parseJSON (Object v) = OrderBook <$> v .: "bids" <*> v .: "asks"
    parseJSON _ = mzero

instance FromJSON OrderBookEntry where
    parseJSON (Object v) = OrderBookEntry <$> rstr v "volume" <*> rstr v "price"
    parseJSON _ = mzero

instance FromJSON TradeHistory where
    parseJSON (Object v) = TradeHistory <$> v .: "trades"
    parseJSON _ = mzero

instance FromJSON Trade where
    parseJSON (Object v) = do
        ts <- v .: "timestamp"
        price <- rstr v "price"
        volume <- rstr v "volume"
        isBuy <- v .: "is_buy"
        case isBuy of
            True -> return $ Trade ts price volume BID
            False -> return $ Trade ts price volume ASK
    parseJSON _ = mzero

instance FromJSON Accounts where
    parseJSON (Object v) = Accounts <$> v .: "balance"
    parseJSON _ = mzero

instance FromJSON Account where
    parseJSON (Object v) =
        Account <$> v .: "account_id"
                <*> v .:? "name"
                <*> (v .: "asset" >>= return . fmap toUpper)
                <*> rstr v "balance"
                <*> rstr v "reserved"
                <*> rstr v "unconfirmed"
    parseJSON _ = mzero

instance FromJSON Orders where
    parseJSON (Object v) = Orders <$> v .:? "orders"
    parseJSON _ = mzero

instance FromJSON Order where
    parseJSON (Object v) =
        Order   <$> (v .: "type" >>= return . read . fmap toUpper)
                <*> v .: "order_id"
                <*> v .: "creation_timestamp"
                <*> rstr v "limit_volume"
                <*> rstr v "limit_price"
    parseJSON _ = mzero

instance FromJSON OrderResponse where
    parseJSON (Object v) = OrderResponse <$> v .: "order_id"
    parseJSON _ = mzero

rstr v k = (v .: k) >>= return . read
