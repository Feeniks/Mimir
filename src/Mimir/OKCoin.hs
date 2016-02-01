{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Mimir.OKCoin(
    module Mimir.Std,
    module Mimir.OKCoin.Types
) where

import Mimir.Types
import Mimir.Std
import Mimir.OKCoin.Types
import Mimir.OKCoin.Instances

import Control.Lens (view, set, at, _Just)
import Crypto.Hash.MD5 (hash)
import qualified Data.ByteString.Char8 as B
import Data.Char (toUpper)
import Data.List (intersperse, sortOn)
import qualified Data.Map as M
import Data.Monoid
import Network.HTTP.Conduit (urlEncodedBody)
import Numeric (showFFloat)

instance HasManager OKCoin where
    getManager = view ocManager

instance Exchange OKCoin where
    type ExchangeM OKCoin = StdM OKCoin

---
--- Utility
---

apiURL :: String -> [(String, String)] -> StdM OKCoin String
apiURL endpoint px = do
    baseURL <- viewStdM ocBaseURL
    key <- viewStdM ocApiKey
    sec <- viewStdM ocApiSecret
    let qstr = mconcat . intersperse "&" . fmap toParam . sortOn fst . M.toList . set (at "api_key") (Just key) . M.fromList $ px
    let sig = fmap toUpper . B.unpack . hash . B.pack $ qstr ++ "&secret_key=" ++ sec
    let qstr' = qstr ++ "&sign=" ++ sig
    return (baseURL ++ endpoint ++ "?" ++ qstr')

toParam :: (String, String) -> String
toParam (k, v) = k ++ "=" ++ v
