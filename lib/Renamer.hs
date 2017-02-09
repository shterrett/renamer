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

matchedPaths :: [FilePath] -> Args -> [FilePath]
matchedPaths paths args = filter matches paths
  where matches = Glob.match (Glob.compile $ fileGlob args)

newPaths :: [FilePath] -> Args -> [FilePath]
newPaths paths args = fmap subPath paths
  where subPath orig = Rx.subRegex (Rx.mkRegex $ target args)
                                   orig
                                   (substitution args)

pathNamePairs :: [FilePath] -> Args -> [(FilePath, FilePath)]
pathNamePairs paths args = zip oldPaths (newPaths oldPaths args)
  where oldPaths = matchedPaths paths args
