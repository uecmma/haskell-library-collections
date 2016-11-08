module System.CLI.Builder.Env
  ( EnvParser
  ) where

-- TODO: support environment parsers

import           Data.Functor.Identity

type EnvParser a = Identity a
