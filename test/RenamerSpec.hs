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

    describe "Matching file paths with given glob" $ do
      it "returns a list of all matching paths" $ do
        (matchedPaths paths args) `shouldBe` ["image_1.png", "image_2.png"]
        where paths = ["image_1.png", "image_2.png", "image_1.jpg"]
              args = Args { fileGlob = "*.png",
                            target = "image_",
                            substitution = "vacation_img_"
                          }
