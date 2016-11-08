{-# LANGUAGE QuasiQuotes #-}

module Control.OperateSpec where

import           Control.Operate
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do

  describe "opdo" $ do

    it "just return an expression" $ property $
      \x ->
        [opdo| const -> x |] == (x :: Int)
        && [opdo| + -> x |] == x

    it "return infix value for left assoc" $ property $
      \x ->
        (==) [opdo| - -> x; 1; 2 |] $ (x :: Int) - 1 - 2

    it "return infix value for right assoc" $ property $
      \x ->
        (==) [opdo| ** -> x; 1; 2 |] $ (x :: Double) ** 1 ** 2

    it "return infix value for function" $ property $
      \x ->
        (==) [opdo| const -> x; "str"; 'c' |] $ (x :: Int) `const` "str" `const` 'c'

    it "should be through type check" $ do
      [opdo| <*> -> pure const; return True; fail "error" |] `shouldBe` Nothing

    it "should allow multiline" $ do
      [opdo| <*> ->
        pure const
        return True
        fail "error"
        |] `shouldBe` Nothing
