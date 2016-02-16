{-# LANGUAGE OverloadedStrings #-}

module Mimir.Bitfinex.Instances() where

import Mimir.Types
import Mimir.Bitfinex.Types

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Char (toUpper)
import Data.Maybe (catMaybes)
import qualified Data.Vector as V

instance FromJSON Ticker where
    parseJSON (Object v) =
        Ticker  <$> fmap (round . (* 1000) . read) (v .: "timestamp")
                <*> rstr v "ask"
                <*> rstr v "bid"
                <*> rstr v "last_price"
    parseJSON _ = mzero

instance FromJSON BFBalance where
    parseJSON (Object v) = BFBalance <$> v .: "type" <*> v .: "currency" <*> rstr v "available"
    parseJSON _ = mzero

rstr v k = (v .: k) >>= return . read
