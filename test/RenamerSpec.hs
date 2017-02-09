module RenamerSpec where

import Test.Hspec

import Renamer

spec :: Spec
spec = do
    describe "Argument Parsing returns a result" $ do
      it "on Success, contains a record of arguments" $ do
        (argValues ["img_", "picture_", "file_1", "file_2", "file_3"]) `shouldBe`
          (Right $ Args { target = "img_",
                          substitution = "picture_",
                          fileNames = ["file_1", "file_2", "file_3"]
                        })
      it "on Failure, contains an error string" $ do
        (argValues []) `shouldBe`
          (Left "Incorrect arguments: <target> <substitution> ... <files>")

    describe "Substitution to generate new paths" $ do
       it "returns a list of all paths with s/target/substitution/g" $ do
         let args = Args { target = "image_",
                           substitution = "vacation_img_",
                           fileNames = ["image_1.png", "image_2.png"]
                          }
         (newPaths args) `shouldBe` ["vacation_img_1.png", "vacation_img_2.png"]

    describe "(Old, New) path pairs" $ do
      it "returns pairs of the old (matching) paths and the new (substituted) paths" $ do
        let args = Args { target = "image_",
                          substitution = "vacation_img_",
                          fileNames = ["image_1.png", "image_2.png"]
                        }
        (pathNamePairs args) `shouldBe`
          [("image_1.png", "vacation_img_1.png"),
           ("image_2.png", "vacation_img_2.png")]
