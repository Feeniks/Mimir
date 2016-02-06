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

data StdErr = StdErr String deriving Show

data Ticker = Ticker {
    _tiTimeUTCMS :: Int,
    _tiAsk :: Double,
    _tiBid :: Double,
    _tiLast :: Double
} deriving Show

type CandleInterval = Int

data Candle = Candle {
    _caTimeUTC :: Int,
    _caOpen :: Double,
    _caClose :: Double,
    _caHigh :: Double,
    _caLow :: Double,
    _caVolume :: Double
} deriving Show

data OrderBook = OrderBook {
    _obBids :: [OrderBookEntry],
    _obAsks :: [OrderBookEntry]
} deriving Show

data OrderBookEntry = OrderBookEntry {
    _oeVolume :: Double,
    _oePrice :: Double
} deriving Show

data Trade = Trade {
    _trTimeUTCMS :: Int,
    _trUnitPrice :: Double,
    _trVolume :: Double,
    _trType :: OrderType
} deriving Show

data Order = Order {
    _oType :: OrderType,
    _oID :: String,
    _oTimeUTCMS :: Int,
    _oVolume :: Double,
    _oUnitPrice :: Double
} deriving Show

data OrderType = ASK | BID deriving (Read,Show,Eq)

data OrderResponse = OrderResponse String deriving Show

data Balances = Balances {
    _bCurrency :: Double,
    _bCommodity :: Double
} deriving Show

makeLenses ''Ticker
makeLenses ''Candle
makeLenses ''OrderBook
makeLenses ''OrderBookEntry
makeLenses ''Trade
makeLenses ''Order
makeLenses ''Balances
