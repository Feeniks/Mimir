{-# LANGUAGE TemplateHaskell #-}

module Mimir.BitX.Types where

import Mimir.Std.Types

import Control.Lens.TH
import Network.HTTP.Conduit (Manager)

data BitX = BitX {
    _bxManager :: Manager,
    _bxBaseURL :: String,
    _bxPairCode :: String,
    _bxCurrencyCode :: String,
    _bxCommodityCode :: String
}

data Account = Account {
    _acID :: String,
    _acName :: Maybe String,
    _acAssetCode :: String,
    _acBalance :: Double,
    _acReserved :: Double,
    _acUnconfirmed :: Double
} deriving Show

data Accounts = Accounts [Account] deriving Show

data PriceHistory = PriceHistory [Candle] deriving Show

data TradeHistory = TradeHistory [Trade] deriving Show

data Orders = Orders (Maybe [Order]) deriving Show

makeLenses ''BitX
makeLenses ''Account
