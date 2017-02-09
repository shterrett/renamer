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
        let paths = ["image_1.png", "image_2.png", "image_1.jpg"]
        let args = Args { fileGlob = "*.png",
                          target = "image_",
                          substitution = "vacation_img_"
                        }
        (matchedPaths args paths) `shouldBe` ["image_1.png", "image_2.png"]

    describe "Substitution to generate new paths" $ do
       it "returns a list of all paths with s/target/substitution/g" $ do
         let paths = ["image_1.png", "image_2.png"]
         let args = Args { fileGlob = "*.png",
                            target = "image_",
                            substitution = "vacation_img_"
                          }
         (newPaths args paths) `shouldBe` ["vacation_img_1.png", "vacation_img_2.png"]

    describe "(Old, New) path pairs" $ do
      it "returns pairs of the old (matching) paths and the new (substituted) paths" $ do
        let paths = ["image_1.png", "image_2.png", "image_1.jpg"]
        let args = Args { fileGlob = "*.png",
                          target = "image_",
                          substitution = "vacation_img_"
                        }
        (pathNamePairs args paths) `shouldBe`
          [("image_1.png", "vacation_img_1.png"),
           ("image_2.png", "vacation_img_2.png")]
