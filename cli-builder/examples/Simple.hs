module Main where

import           Data.Monoid
import           System.CLI.Builder

data Options = Options
  { isSampleOption :: Bool
  , getNameOption  :: String
  }

optionsParser :: OptionParser Options
optionsParser = Options
  <$> switch (long "sample")
  <*> strOption (long "name")

cliInfo :: CLIInfo
cliInfo = baseCLIInfo "Simple CLI" "Example for simple CLI"

-- | Simple CLI example
--
-- Running:
--
-- ```bash
-- $ simple-cli --name yeah
-- Is sample  : False
-- Name value : yeah
-- $ simple-cli --sample --name yeah
-- Is sample  : True
-- Name value : yeah
-- $ simple-cli --help
-- Simple CLI
--
-- Usage: simple-cli [--help] [--sample] --name ARG
--   Example for simple CLI
--
-- Available options:
--   --help                   Show this help text
-- ```
--
main :: IO ()
main = buildSimpleCLI cliInfo optionsParser $ \options -> do
  putStrLn $ "Is sample  : " <> show (isSampleOption options)
  putStrLn $ "Name value : " <> getNameOption options
