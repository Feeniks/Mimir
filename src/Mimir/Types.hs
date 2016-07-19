{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Mimir.Types where

import Control.Lens.Lens (Lens')
import Control.Lens.TH
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Network.HTTP.Nano (HttpCfg, HttpError)

class HasExchange e r where
    exchange :: Lens' r e

class Exchange e where
    type ExchangeM e = (m :: * -> *) | m -> e

class Exchange e => TickerP e where
    type TickerT e :: *
    ticker :: ExchangeM e (TickerT e)

class Exchange e => SpotP e where
    type SpotBalancesT e :: *
    type SpotOrderT e :: *
    type SpotOrderIDT e :: *
    spotBalances :: ExchangeM e (SpotBalancesT e)
    currentSpotOrders :: ExchangeM e [SpotOrderT e]
    placeSpotOrder :: SpotOrderT e -> ExchangeM e (SpotOrderIDT e)
    cancelSpotOrder :: SpotOrderIDT e -> ExchangeM e ()

type TradeM e = ReaderT (Ctx e) (ExceptT TradeError IO)

data Ctx e = Ctx {
    _ctxHttpCfg :: HttpCfg,
    _ctxExchange :: e
}

data TradeError
    = THttpError HttpError
    | TLogicError String
    deriving Show

---
--- Standard data types
---

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
    _oID :: Int,
    _oTimeUTCMS :: Int,
    _oVolume :: Double,
    _oUnitPrice :: Double
} deriving (Eq, Show)

data OrderType
    = LIMIT_BUY
    | LIMIT_SELL
    | MARKET_BUY
    | MARKET_SELL
    deriving (Eq, Read, Show)

data OrderResponse = OrderResponse String deriving (Eq, Show)

data Balances = Balances {
    _bCurrency :: Double,
    _bCommodity :: Double
} deriving (Eq, Show)

makeLenses ''Ctx
makeClassyPrisms ''TradeError
makeLenses ''Ticker
makeLenses ''Candle
makeLenses ''OrderBook
makeLenses ''OrderBookEntry
makeLenses ''Trade
makeLenses ''Order
makeLenses ''Balances
