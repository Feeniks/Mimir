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
    _ssUpdatedUTCMS :: Int,
    _ssCurrencyBalance :: Double,
    _ssCommodityBalance :: Double,
    _ssPendingLimitOrders :: [PendingLimitOrder],
    _ssPendingMarketOrders :: [PendingMarketOrder]
}

data PendingLimitOrder = PendingLimitOrder {
    _ploType :: OrderType,
    _ploID :: String,
    _ploTimeUTCMS :: Int,
    _ploVolume :: Double,
    _ploUnitPrice :: Double
}

data PendingMarketOrder = PendingMarketOrder {
    _pmoType :: OrderType,
    _pmoID :: String,
    _pmoTimeUTCMS :: Int,
    _pmoAmount :: Double
}

makeLenses ''Sim
makeLenses ''SimState
makeLenses ''PendingLimitOrder
makeLenses ''PendingMarketOrder
