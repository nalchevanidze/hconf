module HMM.Core.Env
  ( Env (..),
    defaultConfig,
  )
where

data Env = Env
  { hie :: FilePath,
    hconf :: FilePath,
    stack :: FilePath,
    silence :: Bool
  }

defaultConfig :: Env
defaultConfig =
  Env
    { hconf = "./hconf.yaml",
      hie = "./hie.yaml",
      stack = "./stack.yaml",
      silence = False
    }
