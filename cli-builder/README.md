# CLI Builder [![Hackage page (downloads and API reference)][hackage-png]][hackage]

This packages contains builders to make cli application easily based `optparse-applicative`.

## Getting Started

Here is a simple example:

```haskell
{-# LANGUAGE RecordWildCards #-}

import System.CLI.Builder

data Options = Options
  { isSampleOption :: Bool
  } deriving (Eq, Show)

optionsParser :: OptionParser Options
optionsParser = Options
  <$> switch (long "sample" <> help "Sample switch")

cliInfo :: CLIInfo
cliInfo = baseCLIInfo "Simple CLI" "Example for simple CLI"

run :: Options -> IO ()
run Options{..} = do
  putStrLn "Sample application"
  putStrLn $ "Is sample: " ++ show isSampleOption

main :: IO ()
main = buildSimpleCLI cliInfo optionsParser run
```

This action is such as:

```bash
$ sampleApp
Sample application
Is sample: False
$ sampleApp --sample
Sample application
Is sample: True
$ sampleApp --help
Simple CLI

Usage: <interactive> [--help]
  Example for simple CLI

Available options:
  --help                   Show this help text
  --sample                 Sample switch
```

For more examples, see [examples](examples).

[hackage-png]: https://img.shields.io/hackage/v/cli-builder.svg
[hackage]: https://hackage.haskell.org/package/cli-builder

