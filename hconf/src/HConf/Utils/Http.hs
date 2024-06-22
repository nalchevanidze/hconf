{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Http
  ( hackage,
  )
where

import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.Text as T
import HConf.Utils.Core (Msg (..), Name, maybeToError, throwError)
import Network.HTTP.Req
  ( GET (..),
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

httpRequest :: (FromJSON a, MonadIO m, MonadFail m) => URI -> m (Either String a)
httpRequest uri = case useURI uri of
  Nothing -> throwError ("Invalid Endpoint: " <> msg uri <> "!")
  (Just (Left (u, o))) -> liftIO (eitherDecode . responseBody <$> runReq defaultHttpConfig (req GET u NoReqBody lbsResponse o))
  (Just (Right (u, o))) -> liftIO (eitherDecode . responseBody <$> runReq defaultHttpConfig (req GET u NoReqBody lbsResponse o))

parseURI :: (MonadFail m) => Name -> m URI
parseURI url = maybeToError ("Invalid Endpoint: " <> url <> "!") (mkURI url)

hackage :: (MonadIO m, MonadFail m, FromJSON a) => [Name] -> m (Either String a)
hackage path = parseURI ("https://hackage.haskell.org/" <> T.intercalate "/" path <> ".json") >>= httpRequest
