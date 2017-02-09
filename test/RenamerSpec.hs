module RenamerSpec where

import Test.Hspec

import Renamer

spec :: Spec
spec = do
    describe "Argument Parsing returns a result" $ do
      it "on Success, contains a record of arguments" $ do
        (argValues ["file*", "img_", "picture_"]) `shouldBe`
          (Right $ Args { fileGlob = "file*",
                          target = "img_",
                          substitution = "picture_"
                        })
      it "on Failure, contains an error string" $ do
        (argValues []) `shouldBe`
          (Left "Incorrect arguments: <file path glob> <target> <substitution>")
