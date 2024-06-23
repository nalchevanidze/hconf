{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Http
  ( hackage,
  )
where

import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.Text as T
import HConf.Utils.Core (ErrorMsg, Msg (..), Name, maybeToError, throwError)
import Network.HTTP.Req
  ( GET (..),
    LbsResponse,
    MonadHttp,
    NoReqBody (..),
    defaultHttpConfig,
    lbsResponse,
    req,
    responseBody,
    runReq,
    useURI,
  )
import Relude hiding (ByteString)
import Text.URI (URI, mkURI)

fromUrl :: (MonadFail m, MonadIO m) => URI -> m LbsResponse
fromUrl uri = case useURI uri of
  Nothing -> throwError ("Invalid Endpoint: " <> msg uri <> "!")
  (Just (Left (u, o))) -> runReq defaultHttpConfig $ req GET u NoReqBody lbsResponse o
  (Just (Right (u, o))) -> runReq defaultHttpConfig $ req GET u NoReqBody lbsResponse o

httpRequest :: (FromJSON a, MonadIO m, MonadFail m) => URI -> m (Either ErrorMsg a)
httpRequest = fmap (first msg . eitherDecode . responseBody) . fromUrl

parseURI :: (MonadFail m) => Name -> m URI
parseURI url = maybeToError ("Invalid Endpoint: " <> url <> "!") (mkURI url)

hackage :: (MonadIO m, MonadFail m, FromJSON a) => [Name] -> m (Either ErrorMsg a)
hackage path = parseURI ("https://hackage.haskell.org/" <> T.intercalate "/" path <> ".json") >>= httpRequest
