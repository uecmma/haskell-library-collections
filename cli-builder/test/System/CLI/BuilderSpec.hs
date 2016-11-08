module System.CLI.BuilderSpec where

import           Test.Hspec

spec :: Spec
spec = do

  describe "sample" $ do

    it "should be valid" $ do
      "str" `shouldBe` "str"
