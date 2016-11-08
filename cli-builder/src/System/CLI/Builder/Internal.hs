module System.CLI.Builder.Internal
  ( runCommandExecutes
  , addCommandExecutes
  , fromCommandFields
  , complicatedParser
  , complicatedMonadParser
  , helpOption
  ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Writer
import           Options.Applicative
import           Options.Applicative.Builder.Internal
import           Options.Applicative.Types
import           System.CLI.Builder.Types

addCommandExecutes :: ModCommandFields (c -> a) -> CommandExecutes c a
addCommandExecutes = lift . tell

complicatedParser :: Parser c -> CommandExecutes c a -> Parser a
complicatedParser commonParser commands = commandParser <*> commonParser
  where
    commandParser = runCommandExecutes commands

complicatedMonadParser :: Monad m
  => Parser (m c) -> CommandExecutes c (m a) -> Parser (m a)
complicatedMonadParser commonParser commands = (>>=) <$> commonParser <*> commandParser
  where
    commandParser = runCommandExecutes commands

runCommandExecutes :: CommandExecutes c a -> Parser (c -> a)
runCommandExecutes commands = case runWriter $ runEitherT commands of
  (Right _, d) -> fromCommandFields d
  (Left b , _) -> pure b

fromCommandFields :: ModCommandFields a -> Parser a
fromCommandFields m = mkParser d g rdr
  where
    Mod _ d g = metavar "COMMAND" <> m
    (cmds, subs) = mkCommand m
    rdr = CmdReader cmds (fmap add_helper . subs)
    add_helper pinfo = pinfo
      { infoParser = infoParser pinfo <**> helpOption
      }

helpOption :: Parser (a -> a)
helpOption = abortOption ShowHelpText
  $  long "help"
  <> help "Show this help text"
