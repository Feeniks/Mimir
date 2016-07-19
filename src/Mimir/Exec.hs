{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Mimir.Exec(
    exec
) where

import Mimir.Types
import Mimir.Instances
import Mimir.Free

import Control.Monad.Except (runExceptT)
import Control.Monad.Trans (MonadIO, liftIO)
import Network.HTTP.Nano

exec :: (Exchange e, MonadIO (ExchangeM e)) => Free (Cmd e) a -> ExchangeM e a
exec (Pure a) = return a
exec (Free (ExC cmd) k) = exec . k =<< cmd
exec (Free (LogC msg) k) = exec . k =<< (liftIO . putStrLn) msg
