module System.CLI.Builder.Types
  ( CLIInfo (..)
  , baseCLIInfo
  , ModCommandFields
  , CommandExecutes
  , Parser
  , Middleware
  , MiddlewareIO
  ) where

import           Control.Arrow              hiding (left, right)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer
import           Options.Applicative

data CLIInfo = CLIInfo
  { cliTitle       :: !String
  , cliDescription :: !String
  , cliVersion     :: !(Maybe String)
  , cliFooter      :: !(Maybe String)
  } deriving (Eq, Show)

baseCLIInfo :: String -> String -> CLIInfo
baseCLIInfo title desc = CLIInfo
  { cliTitle       = title
  , cliDescription = desc
  , cliVersion     = Nothing
  , cliFooter      = Nothing
  }

type ModCommandFields a = Mod CommandFields a

type CommandExecutes c a
  = EitherT (c -> a)
  (Writer (ModCommandFields (c -> a)))
  ()

type Middleware c a b = Kleisli (Reader c) a b

type MiddlewareIO c m a b = Kleisli (ReaderT c m) a b
