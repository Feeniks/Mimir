{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Mimir.HTTP(
    Body(..),
    http,
    http',
    httpJSON,
    asJSON,
    buildReq
) where

import Mimir.API

import Control.Exception (SomeException, handle)
import Control.Eff
import Control.Eff.Exception
import Control.Eff.Lift
import Control.Eff.Operational
import Control.Eff.Reader.Strict
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.HTTP.Conduit hiding (http)
import Network.HTTP.Types (Header)

class Body b where
    encodeBody :: b -> BL.ByteString

instance Body BL.ByteString where 
    encodeBody = id

http :: (Member (Lift IO) r, SetMember Lift (Lift IO) r, Member (Exc String) r) => Request -> Manager -> Eff r BL.ByteString
http r mgr = safeHTTP (httpLbs r mgr) >>= return . responseBody

http' :: (Member (Lift IO) r, SetMember Lift (Lift IO) r, Member (Exc String) r) => Request -> Manager -> Eff r ()
http' r mgr = http r mgr >> return ()

httpJSON :: (FromJSON b, Member (Lift IO) r, SetMember Lift (Lift IO) r, Member (Exc String) r) => Request -> Manager -> Eff r b
httpJSON r mgr = http r mgr >>= asJSON

asJSON :: (FromJSON b, Member (Lift IO) r, SetMember Lift (Lift IO) r, Member (Exc String) r) => BL.ByteString -> Eff r b
asJSON b = case (decode b) of
    Nothing -> throwExc $ "Could not parse " ++ BL.unpack b
    Just r -> return r

buildReq :: (Body b, Member (Lift IO) r, SetMember Lift (Lift IO) r, Member (Exc String) r) => String -> B.ByteString -> [Header] -> Maybe b -> Eff r Request
buildReq url mthd headers mb = do
    bres <- lift $ handle handleSome (parseUrl url >>= return . Right)
    case bres of
        Left e -> throwExc e
        Right breq -> do
            let req = breq { method = mthd, requestHeaders = (requestHeaders breq) ++ headers }
            maybe (return req) (\b -> return $ req { requestBody = RequestBodyLBS (encodeBody b) }) mb

safeHTTP :: (Member (Lift IO) r, SetMember Lift (Lift IO) r, Member (Exc String) r) => IO a -> Eff r a
safeHTTP act = do
    res <- lift $ handle handleSome (act >>= return . Right)
    case res of
        Left e -> throwExc e
        Right r -> return r

handleSome :: SomeException -> IO (Either String a)
handleSome ex = return . Left $ show ex
