{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mimir.Instances() where

import Mimir.Types

import Control.Lens (prism)
import Network.HTTP.Nano (AsHttpError(..), HasHttpCfg(..), HttpError)

instance HasExchange e (Ctx e) where
    exchange = ctxExchange

instance HasHttpCfg (Ctx a) where
    httpCfg = ctxHttpCfg

instance AsHttpError TradeError where
    _HttpError = prism THttpError asHttpError

asHttpError :: TradeError -> Either TradeError HttpError
asHttpError (THttpError e) = Right e
asHttpError e = Left e
