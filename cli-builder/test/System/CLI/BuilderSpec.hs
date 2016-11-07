module System.CLI.BuilderSpec where

import           Test.Hspec

spec :: Spec
spec = describe "System.CLI.Builder" $ do

  describe "sample" $ do

    it "should be valid" $ do
      1 `shouldBe` 1
