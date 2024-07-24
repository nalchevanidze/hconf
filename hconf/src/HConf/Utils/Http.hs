{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Http
  ( hackage,
  )
where

import Data.Aeson (FromJSON, eitherDecode)
import HConf.Utils.Class (HConfIO)
import HConf.Utils.Core
  ( Msg (..),
    Name,
    maybeToError,
    throwError,
  )
import HConf.Utils.Source (genUrl)
import Network.HTTP.Req
  ( GET (..),
    LbsResponse,
    NoReqBody (..),
    Option,
    Req,
    Url,
    defaultHttpConfig,
    lbsResponse,
    req,
    responseBody,
    runReq,
    useURI,
  )
import Relude 
import Text.URI (mkURI)

getReq :: (Url s, Option s) -> Req LbsResponse
getReq (u, o) = req GET u NoReqBody lbsResponse o

parse :: (MonadFail m) => Text -> m (Req LbsResponse)
parse url = either getReq getReq <$> maybeToError ("Invalid Endpoint: " <> url <> "!") (mkURI url >>= useURI)

http :: (FromJSON a, HConfIO m) => Text -> m a
http uri = parse uri >>= fmap (first msg . eitherDecode . responseBody) . runReq defaultHttpConfig >>= either throwError pure

hackage :: (HConfIO m, FromJSON a) => [Name] -> m a
hackage path = http (genUrl "https://hackage.haskell.org" path <> ".json")
