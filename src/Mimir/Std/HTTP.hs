{-# LANGUAGE OverloadedStrings #-}

module Mimir.Std.HTTP(
    Body(..),
    http,
    http',
    httpJSON,
    asJSON,
    buildReq
) where

import Mimir.Std.Types

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

http :: HasManager e => Request -> StdM e BL.ByteString
http r = ask >>= return . getManager >>= safeHTTP . httpLbs r >>= return . responseBody

http' :: HasManager e => Request -> StdM e ()
http' r = http r >> return ()

httpJSON :: (HasManager e, FromJSON r) => Request -> StdM e r
httpJSON r = http r >>= asJSON

asJSON :: FromJSON b => BL.ByteString -> StdM e b
asJSON b = case (decode b) of
    Nothing -> lift . left . StdErr $ "Could not parse " ++ BL.unpack b
    Just r -> return r

buildReq :: Body b => String -> B.ByteString -> [Header] -> Maybe b -> StdM e Request
buildReq url mthd headers mb = do
    breq <- parseUrl url
    let req = breq { method = mthd, requestHeaders = (requestHeaders breq) ++ headers }
    maybe (return req) (\b -> return $ req { requestBody = RequestBodyLBS (encodeBody b) }) mb

safeHTTP :: IO a -> StdM e a
safeHTTP act = lift . EitherT $ handle handleHTTP (act >>= return . Right)

handleHTTP :: HttpException -> IO (Either StdErr a)
handleHTTP ex = return . Left . StdErr $ show ex
