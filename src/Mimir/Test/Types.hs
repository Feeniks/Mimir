{-# LANGUAGE TemplateHaskell #-}

module Mimir.Test.Types where

import Control.Lens.TH

data Test e = Test {
    _teExchange :: e,
    _teCurrencyBalance :: Double,
    _teCommodityBalance :: Double
}

makeLenses ''Test
