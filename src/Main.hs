module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
    unlines <$> getArgs >>= putStrLn
