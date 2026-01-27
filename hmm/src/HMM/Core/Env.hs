module HMM.Core.Env
  ( Env (..),
    defaultConfig,
  )
where

data Env = Env
  { hie :: FilePath,
    hmm :: FilePath,
    stack :: FilePath,
    quiet :: Bool
  }

defaultConfig :: Env
defaultConfig =
  Env
    { hmm = "./hmm.yaml",
      hie = "./hie.yaml",
      stack = "./stack.yaml",
      quiet = False
    }
