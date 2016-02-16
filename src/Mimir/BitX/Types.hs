{-# LANGUAGE TemplateHaskell #-}

module Mimir.BitX.Types where

import Control.Lens.TH
import Data.Typeable
import qualified Network.HTTP.Conduit as HTTPC

data BitX = BitX {
    _bxManager :: HTTPC.Manager,
    _bxBaseURL :: String,
    _bxPairCode :: String
} deriving Typeable

makeLenses ''BitX
