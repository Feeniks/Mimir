{-# LANGUAGE TemplateHaskell #-}

module Mimir.Std.Types where

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

type PriceInterval = Int

data PriceSample = PriceSample {
    _psTimeUTC :: Int,
    _psOpen :: Double,
    _psClose :: Double,
    _psHigh :: Double,
    _psLow :: Double,
    _psVolume :: Double
} deriving Show

data OrderBook = OrderBook {
    _oBids :: [OrderBookEntry],
    _oAsks :: [OrderBookEntry]
} deriving Show

data OrderBookEntry = OrderBookEntry {
    _oeVolume :: Double,
    _oePrice :: Double
} deriving Show

data Trade = Trade {
    _trTimeUTCMS :: Maybe Int,
    _trUnitPrice :: Double,
    _trVolume :: Double
} deriving Show

data Order = Order {
    _oType :: OrderType,
    _oID :: String,
    _oTimeUTC :: Maybe Int,
    _oExpirationTimeUTC :: Maybe Int,
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
makeLenses ''PriceSample
makeLenses ''OrderBook
makeLenses ''OrderBookEntry
makeLenses ''Trade
makeLenses ''Order
makeLenses ''Balances
