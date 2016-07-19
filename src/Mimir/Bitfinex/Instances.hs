{-# LANGUAGE OverloadedStrings #-}

module Mimir.Bitfinex.Instances() where

import Mimir.Types
import Mimir.Bitfinex.Types

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Char (toUpper)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes)
import qualified Data.Text as T

instance FromJSON Ticker where
    parseJSON (Object v) =
        Ticker  <$> fmap (round . (* 1000) . read) (v .: "timestamp")
                <*> (readSafe =<< v .: "ask")
                <*> (readSafe =<< v .: "bid")
                <*> (readSafe =<< v .: "last_price")
    parseJSON _ = mzero

instance FromJSON BFBalance where
    parseJSON (Object v) =
            BFBalance   <$> v .: "type"
                        <*> v .: "currency"
                        <*> (readSafe =<< v .: "available")
    parseJSON _ = mzero

instance FromJSON Order where
    parseJSON (Object v) =
        Order   <$> parseType v
                <*> v .: "id"
                <*> (round <$> parseDouble v "timestamp")
                <*> parseDouble v "original_amount"
                <*> parseDouble v "price"
    parseJSON _ = mzero

parseType :: HM.HashMap T.Text Value -> Parser OrderType
parseType m = do
    t <- (m .: "type" :: Parser String)
    s <- (m .: "side" :: Parser String)
    case (t,s) of
        ("exchange limit", "buy") -> return LIMIT_BUY
        ("exchange limit", "sell") -> return LIMIT_SELL
        ("exchange market", "buy") -> return MARKET_BUY
        ("exchange market", "sell") -> return MARKET_SELL
        otherwise -> mzero

parseDouble :: HM.HashMap T.Text Value -> T.Text -> Parser Double
parseDouble m k = do
    s <- m .: k
    case (readsPrec 0 s) of
        [(v,_)] -> return v
        _ -> mzero

readSafe :: Read a => String -> Parser a
readSafe s = case readsPrec 0 s of
    [(a, _)] -> return a
    _ -> mzero
