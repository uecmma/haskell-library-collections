module System.CLI.Builder.Env
  ( EnvParser
  ) where

import           Data.Functor.Identity

type EnvParser a = Identity a
