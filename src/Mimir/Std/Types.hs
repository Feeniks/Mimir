{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Mimir.Std.Types where

import Mimir.Types

import Control.Lens.TH
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Network.HTTP.Conduit (Manager)

type StdM s = ReaderT s (EitherT StdErr IO)

class HasManager e where
    getManager :: e -> Manager

data StdErr = StdErr String deriving (Eq, Show)

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

data OrderResponse = OrderResponse String deriving (Eq, Show)

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
