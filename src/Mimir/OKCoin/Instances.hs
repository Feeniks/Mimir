{-# LANGUAGE OverloadedStrings #-}

module Mimir.OKCoin.Instances() where

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

instance Show OKCandleInterval where
    show M1 = "1min"
    show M3 = "3min"
    show M5 = "5min"
    show M15 = "15min"
    show M30 = "30min"
    show H1 = "1hour"
    show H2 = "2hour"
    show H4 = "4hour"
    show H6 = "6hour"
    show H12 = "12hour"
    show D1 = "1day"
    show D3 = "3day"
    show W1 = "1week"

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
    parseJSON (Array sx) = PriceHistory <$> (mapM toCandle $ V.toList sx)
        where
        toCandle (Array vx) = case V.length vx of
            6 -> do
                let (Number timeMS) = vx ! 0
                let (Number open) = vx ! 1
                let (Number high) = vx ! 2
                let (Number low) = vx ! 3
                let (Number close) = vx ! 4
                let (Number volume) = vx ! 5
                return $ Candle (truncate $ timeMS / 1000) (realToFrac open) (realToFrac close) (realToFrac high) (realToFrac low) (realToFrac volume)
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

instance FromJSON Trade where
    parseJSON (Object v) = do
        ts <- fmap Just (v .: "date_ms")
        price <- rstr v "price"
        volume <- rstr v "amount"
        return $ Trade ts price volume
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

instance FromJSON OKBalances where
    parseJSON (Object v) = do
        vinf <- v .: "info"
        vfunds <- vinf .: "funds"
        vfree <- vfunds .: "free"
        usd <- rstr vfree "usd"
        btc <- rstr vfree "btc"
        ltc <- rstr vfree "ltc"
        return $ OKBalances [("usd", usd), ("btc", btc), ("ltc", ltc)]
    parseJSON _ = mzero

rstr v k = (v .: k) >>= return . read
