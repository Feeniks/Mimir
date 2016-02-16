{-# LANGUAGE TemplateHaskell #-}

module Mimir.Bitfinex.Types where

import Mimir.Types

import Control.Concurrent.STM.TVar (TVar)
import Control.Lens.TH
import Network.HTTP.Conduit (Manager)

data Bitfinex = Bitfinex {
    _bfNonce :: TVar Int,
    _bfManager :: Manager,
    _bfBaseURL :: String,
    _bfVersionCode :: String,
    _bfApiKey :: String,
    _bfApiSecret :: String,
    _bfSymbol :: String,
    _bfCurrencySymbol :: String,
    _bfCommoditySymbol :: String
}

data BFBalance = BFBalance String String Double deriving Show

makeLenses ''Bitfinex
