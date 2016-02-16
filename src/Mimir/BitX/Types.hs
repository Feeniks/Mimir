{-# LANGUAGE TemplateHaskell #-}

module Mimir.BitX.Types where

import Mimir.Types

import Control.Lens.TH
import Data.Typeable
import qualified Network.HTTP.Conduit as HTTPC

data BitX = BitX {
    _bxManager :: HTTPC.Manager,
    _bxBaseURL :: String,
    _bxPairCode :: String,
    _bxCurrencyCode :: String,
    _bxCommodityCode :: String
} deriving Typeable

data Account = Account {
    _acID :: String,
    _acName :: Maybe String,
    _acAssetCode :: String,
    _acBalance :: Double,
    _acReserved :: Double,
    _acUnconfirmed :: Double
} deriving Show

data Accounts = Accounts { acsAccounts :: [Account] } deriving Show

data TradeHistory = TradeHistory { thTrades :: [Trade] } deriving Show

data Orders = Orders { orsOrders :: Maybe [Order] } deriving Show

makeLenses ''BitX
makeLenses ''Account
