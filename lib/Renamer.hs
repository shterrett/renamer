module Renamer where

import System.IO (FilePath)
import qualified Text.Regex as Rx

data Args = Args { target :: String,
                   substitution :: String,
                   fileNames :: [FilePath]
                 } deriving (Eq, Show)

argValues :: [String] -> Either String Args
argValues (t:s:fs) = Right $ Args { target = t,
                                    substitution = s,
                                    fileNames = fs
                                  }
argValues _ = Left "Incorrect arguments: <target> <substitution> ... <files>"

newPaths :: Args -> [FilePath]
newPaths args = fmap subPath $ fileNames args
  where subPath orig = Rx.subRegex (Rx.mkRegex $ target args)
                                   orig
                                   (substitution args)

pathNamePairs :: Args -> [(FilePath, FilePath)]
pathNamePairs args = zip (fileNames args) (newPaths args)
