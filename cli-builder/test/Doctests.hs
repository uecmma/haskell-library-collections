module Main where

import           Data.Semigroup
import           System.FilePath
import           System.FilePath.Find
import           Test.DocTest

main :: IO ()
main = do
  files <- find always (extension ==? ".hs") "src"
  tfiles <- find always (extension ==? ".hs") "test"
  doctest $
    [ "-isrc"
    , "-XOverloadedStrings"
    , "-XFlexibleInstances"
    , "-XMultiParamTypeClasses"
    , "-XTemplateHaskell"
    , "-XQuasiQuotes"
    ]
    <> files
    <> filter (`notElem` ["test/Doctests.hs"]) tfiles
