module System.CLI.Builder.Option
  ( OptionParser
  , module OptionsApplicative
  ) where

import           Options.Applicative as OptionsApplicative hiding (Parser)
import           Options.Applicative (Parser)

type OptionParser a = Parser a
