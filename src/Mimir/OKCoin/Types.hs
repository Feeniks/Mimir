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
    _ocSymbol :: String
}

data OKPriceInterval =
    M1 | M3 | M5 | M15 | M30 |
    H1 | H2 | H4 | H6 | H12 |
    D1 | D3 |
    W1

data PriceHistory = PriceHistory [PriceSample] deriving Show

data Orders = Orders [Order] deriving Show

makeLenses ''OKCoin
