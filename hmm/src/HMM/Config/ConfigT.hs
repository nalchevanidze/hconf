{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HMM.Config.ConfigT
  ( ConfigT (..),
    HCEnv (..),
    run,
    runTask,
    runUpdate,
    VersionMap,
  )
where

import Control.Exception (tryJust)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import HMM.Config.Build (Builds, allDeps)
import HMM.Config.Config (Config (..), getRule)
import HMM.Config.PkgGroup (pkgDirs)
import HMM.Core.Bounds (Bounds)
import HMM.Core.Env (Env (..))
import HMM.Core.HkgRef (VersionMap, Versions, VersionsMap)
import HMM.Core.PkgDir (PkgDirs)
import HMM.Core.Version (Version)
import HMM.Utils.Chalk (Color (Green), chalk)
import HMM.Utils.Class
  ( Check (..),
    Format (format),
    HIO (..),
  )
import HMM.Utils.Core (DependencyName (..), getField, printException)
import HMM.Utils.FromConf (ByKey (..), ReadFromConf (..))
import HMM.Utils.Http (hackage)
import HMM.Utils.Log
  ( alert,
    task,
  )
import HMM.Utils.Yaml (readYaml, rewrite)
import Relude

data HCEnv = HCEnv
  { config :: Config,
    env :: Env,
    indention :: Int,
    versionsMap :: VersionsMap
  }

newtype ConfigT (a :: Type) = ConfigT {_runConfigT :: ReaderT HCEnv IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader HCEnv,
      MonadIO,
      MonadFail
    )

runConfigT :: ConfigT a -> Env -> Config -> VersionsMap -> IO (Either String a)
runConfigT (ConfigT (ReaderT f)) env config versionsMap = tryJust (Just . printException) (f HCEnv {indention = 0, ..})

indent :: Int -> String -> String
indent i = (replicate (i * 2) ' ' <>)

instance HIO ConfigT where
  read = liftIO . read
  write f = liftIO . write f
  remove = liftIO . remove
  putLine txt = asks indention >>= liftIO . putLine . flip indent txt
  inside f m = do
    asks indention >>= putLine . f
    local (\c -> c {indention = indention c + 1}) m

fetchVersions :: (HIO m) => DependencyName -> m (DependencyName, Versions)
fetchVersions name = do
  vs <- hackage ["package", format name, "preferred"] >>= getField "normal-version"
  pure (name, vs)

prefetchVersionsMap :: (HIO m) => Config -> m VersionsMap
prefetchVersionsMap cfg = do
  let extras = toList (Set.fromList $ concatMap allDeps (builds cfg))
  ps <- traverse fetchVersions extras
  pure (Map.fromList ps)

computeConfigHash :: Config -> Text
computeConfigHash cfg =
  let hashInput = T.encodeUtf8 (T.pack (show cfg))
      hashBytes = SHA256.hash hashInput
   in T.decodeUtf8 (Base16.encode hashBytes)

extractHashFromFile :: FilePath -> IO (Maybe Text)
extractHashFromFile filePath = do
  content <- T.decodeUtf8 <$> readFileBS filePath
  case T.lines content of
    (firstLine : _) ->
      case T.stripPrefix "# hash: " firstLine of
        Just hash -> pure (Just hash)
        Nothing -> pure Nothing
    [] -> pure Nothing

isConfigChanged :: Config -> FilePath -> IO Bool
isConfigChanged cfg filePath = do
  storedHash <- extractHashFromFile filePath
  let currentHash = computeConfigHash cfg
  case storedHash of
    Nothing -> pure True -- No hash means we should do full check
    Just hash -> pure (hash /= currentHash)

run :: (ToString a) => Bool -> ConfigT (Maybe a) -> Env -> IO ()
run fast m env@Env {..}
  | fast = do
      cfg <- readYaml hmm
      runConfigT m env cfg Map.empty >>= handle
  | otherwise = do
      cfg <- readYaml hmm
      changed <- isConfigChanged cfg hmm
      if changed
        then do
          -- Config changed, do full check with HTTP calls
          deps <- prefetchVersionsMap cfg
          runConfigT (asks config >>= check >> save >> m) env cfg deps >>= handle
        else do
          -- Config unchanged, skip expensive operations
          runConfigT m env cfg Map.empty >>= handle

runTask :: Bool -> String -> ConfigT () -> Env -> IO ()
runTask fast name m = run fast (task name m $> Just (chalk Green "\nOk"))

runUpdate :: Bool -> String -> (Config -> ConfigT Config) -> ConfigT () -> Env -> IO ()
runUpdate fast name f m = run fast (task name (localConfig f >> m) $> Just (chalk Green "\nOk"))

handle :: (ToString a) => (HIO m) => Either String (Maybe a) -> m ()
handle res = case res of
  Left x -> do
    alert ("ERROR: " <> x)
    liftIO exitFailure
  (Right Nothing) -> pure ()
  (Right (Just msg)) -> putLine (toString msg)

save :: ConfigT ()
save = task "save" $ task "hmm.yaml" $ do
  cfg <- asks config
  ctx <- asks id
  let hash = computeConfigHash cfg
      filePath = hmm $ env ctx
  -- Write the config normally first
  rewrite filePath (const $ pure cfg) $> ()
  -- Then prepend the hash comment
  content <- liftIO $ T.decodeUtf8 <$> readFileBS filePath
  let contentWithHash = "# hash: " <> hash <> "\n" <> content
  liftIO $ writeFileBS filePath (T.encodeUtf8 contentWithHash)

localConfig :: (Config -> ConfigT Config) -> ConfigT ()
localConfig f = do
  cfg <- asks config
  updatedCfg <- f cfg
  local (\env -> env {config = updatedCfg}) save

instance ReadFromConf ConfigT PkgDirs where
  readFromConf = const $ concatMap pkgDirs <$> asks (groups . config)

instance ReadFromConf ConfigT Builds where
  readFromConf = const $ asks (builds . config)

instance ReadFromConf ConfigT Env where
  readFromConf = const $ asks env

instance ReadFromConf ConfigT Version where
  readFromConf = const $ asks (version . config)

instance ReadFromConf ConfigT (ByKey DependencyName Bounds) where
  readFromConf name = ByKey <$> (asks config >>= getRule name)

instance ReadFromConf ConfigT VersionsMap where
  readFromConf _ = asks versionsMap
