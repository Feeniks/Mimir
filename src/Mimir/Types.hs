{-# LANGUAGE TemplateHaskell #-}

module Mimir.Types where

import Control.Lens.TH

type Volume = Double

type UnitPrice = Double

data Ticker = Ticker {
    _tiTimeUTCMS :: Int,
    _tiAsk :: Double,
    _tiBid :: Double,
    _tiLast :: Double
} deriving (Eq, Show)

type CandleInterval = Int

data Candle = Candle {
    _caTimeUTC :: Int,
    _caOpen :: Double,
    _caClose :: Double,
    _caHigh :: Double,
    _caLow :: Double,
    _caVolume :: Double
} deriving (Eq, Show)

data OrderBook = OrderBook {
    _obBids :: [OrderBookEntry],
    _obAsks :: [OrderBookEntry]
} deriving (Eq, Show)

data OrderBookEntry = OrderBookEntry {
    _oeVolume :: Double,
    _oePrice :: Double
} deriving (Eq, Show)

data Trade = Trade {
    _trTimeUTCMS :: Int,
    _trUnitPrice :: Double,
    _trVolume :: Double,
    _trType :: OrderType
} deriving (Eq, Show)

data Order = Order {
    _oType :: OrderType,
    _oID :: String,
    _oTimeUTCMS :: Int,
    _oVolume :: Double,
    _oUnitPrice :: Double
} deriving (Eq, Show)

data OrderType = ASK | BID deriving (Read,Show,Eq)

data OrderResponse = OrderResponse { orID :: String } deriving (Eq, Show)

data Balances = Balances {
    _bCurrency :: Double,
    _bCommodity :: Double
} deriving (Eq, Show)

makeLenses ''Ticker
makeLenses ''Candle
makeLenses ''OrderBook
makeLenses ''OrderBookEntry
makeLenses ''Trade
makeLenses ''Order
makeLenses ''Balances
