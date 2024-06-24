{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Utils.Http
  ( hackage,
  )
where

import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.Text as T
import HConf.Utils.Core
  ( Msg (..),
    Name,
    maybeToError,
    throwError,
  )
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
import Text.URI (mkURI)

decodeUrl :: (MonadHttp p) => Either (Url s, Option s) (Url s', Option s') -> p LbsResponse
decodeUrl (Left (u, o)) = req GET u NoReqBody lbsResponse o
decodeUrl (Right (u, o)) = req GET u NoReqBody lbsResponse o

parse :: (MonadFail m, MonadHttp p) => Text -> m (p LbsResponse)
parse url = decodeUrl <$> maybeToError ("Invalid Endpoint: " <> url <> "!") (mkURI url >>= useURI)

http :: (FromJSON a, MonadIO m, MonadFail m) => Text -> m a
http uri = parse uri >>= fmap (first msg . eitherDecode . responseBody) . runReq defaultHttpConfig >>= either throwError pure

hackage :: (MonadIO m, MonadFail m, FromJSON a) => [Name] -> m a
hackage path = http ("https://hackage.haskell.org/" <> T.intercalate "/" path <> ".json")
