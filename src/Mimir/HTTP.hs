{-# LANGUAGE OverloadedStrings #-}

module Mimir.HTTP(
    Body(..),
    http,
    http',
    httpJSON,
    asJSON,
    buildReq
) where

import Mimir.API

import Control.Exception (handle)
import Control.Monad
import Control.Monad.Reader (ask)
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.HTTP.Client (HttpException(..))
import Network.HTTP.Conduit hiding (http)
import Network.HTTP.Types (Header)

class Body b where
    encodeBody :: b -> BL.ByteString

http :: HasManager e => Request -> TradeM e BL.ByteString
http r = ask >>= return . getManager >>= safeHTTP . httpLbs r >>= return . responseBody

http' :: HasManager e => Request -> TradeM e ()
http' r = http r >> return ()

httpJSON :: (HasManager e, FromJSON r) => Request -> TradeM e r
httpJSON r = http r >>= asJSON

asJSON :: FromJSON b => BL.ByteString -> TradeM e b
asJSON b = case (decode b) of
    Nothing -> lift . left . TradeErr $ "Could not parse " ++ BL.unpack b
    Just r -> return r

buildReq :: Body b => String -> B.ByteString -> [Header] -> Maybe b -> TradeM e Request
buildReq url mthd headers mb = do
    breq <- parseUrl url
    let req = breq { method = mthd, requestHeaders = (requestHeaders breq) ++ headers }
    maybe (return req) (\b -> return $ req { requestBody = RequestBodyLBS (encodeBody b) }) mb

safeHTTP :: IO a -> TradeM e a
safeHTTP act = lift . EitherT $ handle handleHTTP (act >>= return . Right)

handleHTTP :: HttpException -> IO (Either TradeErr a)
handleHTTP ex = return . Left . TradeErr $ show ex
