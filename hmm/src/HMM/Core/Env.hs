module HMM.Core.Env
  ( Env (..),
    defaultConfig,
  )
where

data Env = Env
  { hie :: FilePath,
    hmm :: FilePath,
    stack :: FilePath,
    silence :: Bool
  }

defaultConfig :: Env
defaultConfig =
  Env
    { hmm = "./hmm.yaml",
      hie = "./hie.yaml",
      stack = "./stack.yaml",
      silence = False
    }
