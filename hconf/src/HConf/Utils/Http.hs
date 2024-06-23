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
    Option,
    Url,
    defaultHttpConfig,
    lbsResponse,
    req,
    responseBody,
    runReq,
    useURI,
  )
import Relude hiding (ByteString)
import Text.URI (URI, mkURI)

decodeUrl :: (MonadHttp m) => Either (Url s, Option s) (Url s', Option s') -> m LbsResponse
decodeUrl (Left (u, o)) = req GET u NoReqBody lbsResponse o
decodeUrl (Right (u, o)) = req GET u NoReqBody lbsResponse o

fromUrl :: (MonadFail m, MonadHttp p) => URI -> m (p LbsResponse)
fromUrl uri = decodeUrl <$> maybeToError ("Invalid Endpoint: " <> msg uri <> "!") (useURI uri)

httpRequest :: (FromJSON a, MonadIO m, MonadFail m) => URI -> m (Either ErrorMsg a)
httpRequest uri = fromUrl uri >>= fmap (first msg . eitherDecode . responseBody) . runReq defaultHttpConfig

parseURI :: (MonadFail m) => Name -> m URI
parseURI url = maybeToError ("Invalid Endpoint: " <> url <> "!") (mkURI url)

hackage :: (MonadIO m, MonadFail m, FromJSON a) => [Name] -> m (Either ErrorMsg a)
hackage path = parseURI ("https://hackage.haskell.org/" <> T.intercalate "/" path <> ".json") >>= httpRequest
