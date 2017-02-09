module Renamer where

import System.IO (FilePath)
import qualified Text.Regex as Rx

data Args = Args { target :: String,
                   substitution :: String,
                   fileNames :: [FilePath]
                 } deriving (Eq, Show)

argValues :: [String] -> Either String Args
argValues args = if length args < 3
                 then Left "Incorrect arguments: <target> <substitution> ... <files>"
                 else Right $ Args { target = args !! 0,
                                     substitution = args !! 1,
                                     fileNames = drop 2 args
                                   }

newPaths :: Args -> [FilePath]
newPaths args = fmap subPath $ fileNames args
  where subPath orig = Rx.subRegex (Rx.mkRegex $ target args)
                                   orig
                                   (substitution args)

pathNamePairs :: Args -> [(FilePath, FilePath)]
pathNamePairs args = zip (fileNames args) (newPaths args)
