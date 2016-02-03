{-# LANGUAGE TemplateHaskell #-}

module Mimir.Sim.Types where

import Control.Lens.TH

data Sim e = Sim {
    _siExchange :: e,
    _siCurrencyBalance :: Double,
    _siCommodityBalance :: Double
}

makeLenses ''Sim
