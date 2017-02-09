module Renamer where

import System.IO (FilePath)
import qualified System.FilePath.Glob as Glob
import qualified Text.Regex as Rx
import Data.Maybe (isJust)

data Args = Args { fileGlob :: String,
                   target :: String,
                   substitution :: String
                 } deriving (Eq, Show)

argValues :: [String] -> Either String Args
argValues args = if length args /= 3
                 then Left "Incorrect arguments: <file path glob> <target> <substitution>"
                 else Right $ Args { fileGlob = args !! 0,
                                     target = args !! 1,
                                     substitution = args !! 2
                                   }

matchedPaths :: Args -> [FilePath] -> [FilePath]
matchedPaths args = filter matches
  where matches = Glob.match (Glob.compile $ fileGlob args)

newPaths :: Args -> [FilePath] -> [FilePath]
newPaths args = fmap subPath
  where subPath orig = Rx.subRegex (Rx.mkRegex $ target args)
                                   orig
                                   (substitution args)

pathNamePairs :: Args -> [FilePath] -> [(FilePath, FilePath)]
pathNamePairs args paths = zip oldPaths (newPaths args oldPaths)
  where oldPaths = matchedPaths args paths
