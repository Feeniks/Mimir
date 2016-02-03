{-# LANGUAGE TemplateHaskell #-}

module Mimir.Sim.Types where

import Mimir.Std.Types

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM.TVar (TVar)
import Control.Lens.TH

data Sim e = Sim {
    _siExchange :: e,
    _siManagerThreadID :: ThreadId,
    _siState :: TVar SimState
}

data SimState = SimState {
    _ssIDGen :: Int,
    _ssCurrencyBalance :: Double,
    _ssCommodityBalance :: Double,
    _ssPendingLimitOrders :: [PendingLimitOrder]
}

data PendingLimitOrder = PendingLimitOrder {
    _ploType :: OrderType,
    _ploID :: String,
    _ploTimeUTCUS :: Maybe Int,
    _ploExpirationTimeUTCUS :: Maybe Int,
    _ploVolume :: Double,
    _ploUnitPrice :: Double,
    _ploOutstanding :: Double
}

makeLenses ''Sim
makeLenses ''SimState
makeLenses ''PendingLimitOrder
