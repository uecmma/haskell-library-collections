{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module System.CLI.Builder
  ( CommandExecutes
  , module System.CLI.Builder.Option
  , CLIInfo (..)
  , baseCLIInfo
  , buildCLIApp
  , buildCLIGeneralApp
  , buildSimpleCLI
  , buildCLI
  , cmdProgram
  , Middleware
  , addCommandExecutes
  , cmdMiddleware
  , cmdCommonMiddleware
  , addCommand
  , addSimpleCommand
  , MiddlewareIO
  , cmdMiddlewareIO
  , cmdCommonMiddlewareIO
  , addCommandIO
  , addSimpleCommandIO
  ) where

import           Control.Applicative
import           Control.Arrow               hiding (left, right)
import           Control.Category
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader
import           Options.Applicative
import           Options.Applicative.Types
import           Prelude                     hiding (id, (.))
import           System.CLI.Builder.Internal
import           System.CLI.Builder.Option
import           System.CLI.Builder.Types
import           System.Environment

-- | a builder for CLI application
--
-- Examples:
-- >>> :{
-- let cliInfo = baseCLIInfo "CLI Application" "Example of CLI Application"
-- in withArgs ["--help"] $ buildCLIApp cliInfo return (pure ()) $ do
--      cmdProgram $ \_ ->
--        putStrLn "Hello, World!"
-- :}
-- CLI Application
-- <BLANKLINE>
-- Usage: <interactive> [--help]
--   Example of CLI Application
-- <BLANKLINE>
-- Available options:
--   --help                   Show this help text
-- *** Exception: ExitSuccess
--
-- >>> :{
-- let cliInfo = baseCLIInfo "CLI Application" "Example of CLI Application"
-- in withArgs [] $ buildCLIApp cliInfo return (pure ()) $ do
--      cmdProgram $ \_ ->
--        putStrLn "Hello, World!"
-- :}
-- Hello, World!
--
-- >>> :{
-- let cliInfo = baseCLIInfo "CLI Application" "Example of CLI Application"
-- in withArgs ["--illegal"] $ buildCLIApp cliInfo return (pure ()) $ do
--      cmdProgram $ \_ ->
--        putStrLn "Hello, World!"
-- :}
-- Invalid option `--illegal'
-- <BLANKLINE>
-- Usage: <interactive> [--help]
--   Example of CLI Application
-- *** Exception: ExitFailure 1
--
buildCLIApp
  :: MonadIO m
  => CLIInfo
  -> (c -> m d)
  -> Parser c
  -> CommandExecutes d (m a)
  -> m a
buildCLIApp cliInfo middleware commonParser commands
  = join $ buildCLI cliInfo Nothing parser
  where
    parser = complicatedMonadParser
      (middleware <$> commonParser)
      commands

buildCLIGeneralApp
  :: MonadIO m
  => CLIInfo
  -> Parser c
  -> Maybe (ParserFailure ParserHelp -> [String] -> m a)
  -> CommandExecutes c a
  -> m a
buildCLIGeneralApp cliInfo commonParser mOnFailure commands
  = buildCLI cliInfo mOnFailure parser
  where
    parser = complicatedParser commonParser commands

-- | a builder for simple CLI application
--
-- Examples:
-- >>> :{
-- let cliInfo = baseCLIInfo "Simple CLI Application" "Example of Simple CLI Application"
-- in withArgs ["--help"] $ buildSimpleCLI cliInfo (pure ()) $ \x -> do
--      putStrLn "Hello, World!"
-- :}
-- Simple CLI Application
-- <BLANKLINE>
-- Usage: <interactive> [--help]
--   Example of Simple CLI Application
-- <BLANKLINE>
-- Available options:
--   --help                   Show this help text
-- *** Exception: ExitSuccess
--
-- >>> :{
-- let cliInfo = baseCLIInfo "Simple CLI Application" "Example of Simple CLI Application"
-- in withArgs [] $ buildSimpleCLI cliInfo (pure ()) $ \x -> do
--      putStrLn "Hello, World!"
-- :}
-- Hello, World!
--
-- >>> :{
-- let cliInfo = baseCLIInfo "Simple CLI Application" "Example of Simple CLI Application"
-- in withArgs ["--illegal"] $ buildSimpleCLI cliInfo (pure ()) $ \x -> do
--      putStrLn "Hello, World!"
-- :}
-- Invalid option `--illegal'
-- <BLANKLINE>
-- Usage: <interactive> [--help]
--   Example of Simple CLI Application
-- *** Exception: ExitFailure 1
--
buildSimpleCLI
  :: MonadIO m
  => CLIInfo
  -> Parser c
  -> (c -> m a)
  -> m a
buildSimpleCLI cliInfo parser cmd
  = join $ buildCLI cliInfo Nothing $ cmd <$> parser

buildCLI
  :: MonadIO m
  => CLIInfo
  -> Maybe (ParserFailure ParserHelp -> [String] -> m a)
  -> Parser a
  -> m a
buildCLI CLIInfo{..} mOnFailure optParser = do
  args <- liftIO getArgs

  result <- case execParserPure (prefs noBacktrack) parser args of
    Failure _   | null args -> liftIO $ withArgs ["--help"] $ execParser parser
    Failure f   | Just onFailure <- mOnFailure -> onFailure f args
    parseResult -> do
      liftIO $ handleParseResult parseResult

  return result
  where
    parser = info
      (   helpOption
      <*> mayVersionOption
      <*> optParser
      )
      $  fullDesc
      <> header cliTitle
      <> progDesc cliDescription
      <> maybe mempty footer cliFooter

    mayVersionOption = maybe (pure id) versionOption cliVersion

    versionOption verStr = infoOption verStr
      $  long "version"
      <> help "Show version"

cmdProgram :: (c -> a) -> CommandExecutes c a
cmdProgram = left

cmdMiddleware :: (a -> b) -> Middleware c a b
cmdMiddleware = arr

cmdCommonMiddleware :: (a -> c -> b) -> Middleware c a b
cmdCommonMiddleware f = Kleisli $ \a -> f a <$> ask

addCommand :: String -> String -> Parser a -> Middleware c a b -> CommandExecutes c b
addCommand name desc parser middleware
  = addCommandExecutes $ command name $
    info (runReader . runKleisli middleware <$> parser)
    $ progDesc desc

addSimpleCommand :: String -> String -> Parser a -> (a -> c -> b) -> CommandExecutes c b
addSimpleCommand name desc parser
  = addCommand name desc parser . cmdCommonMiddleware

cmdMiddlewareIO :: MonadIO m => (a -> m b) -> MiddlewareIO c m a b
cmdMiddlewareIO f = Kleisli $ lift . f

cmdCommonMiddlewareIO :: MonadIO m => (a -> c -> m b) -> MiddlewareIO c m a b
cmdCommonMiddlewareIO f = Kleisli $ \a -> ask >>= (lift . f a)

addCommandIO :: MonadIO m
  => String -> String -> Parser a -> MiddlewareIO c m a b -> CommandExecutes c (m b)
addCommandIO name desc parser middleware
  = addCommandExecutes $ command name $
    info (runReaderT . runKleisli middleware <$> parser)
    $ progDesc desc

addSimpleCommandIO :: MonadIO m
  => String -> String -> Parser a -> (a -> c -> m b) -> CommandExecutes c (m b)
addSimpleCommandIO name desc parser
  = addCommandIO name desc parser . cmdCommonMiddlewareIO
