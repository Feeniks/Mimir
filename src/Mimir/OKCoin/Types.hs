{-# LANGUAGE TemplateHaskell #-}

module Mimir.OKCoin.Types where

import Mimir.Std.Types

import Control.Lens.TH
import Network.HTTP.Conduit (Manager)

data OKCoin = OKCoin {
    _ocManager :: Manager,
    _ocBaseURL :: String,
    _ocApiKey :: String,
    _ocApiSecret :: String,
    _bxSymbol :: String
}

data PriceHistory = PriceHistory [PriceSample] deriving Show

data Orders = Orders [Order] deriving Show

makeLenses ''OKCoin
