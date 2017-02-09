module Renamer where

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
