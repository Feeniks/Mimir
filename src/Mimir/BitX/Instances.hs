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

rstr v k = (v .: k) >>= return . read
