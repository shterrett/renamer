module Main where

import System.IO (FilePath)
import qualified System.Directory as Dir
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Renamer

main :: IO ()
main = do
    argValues <$> getArgs >>= runRenamer

runRenamer :: Either String Args -> IO ()
runRenamer (Left error) = putStrLn error >> exitFailure
runRenamer (Right args) = renameFiles (pathNamePairs args) >>
                            exitSuccess

renameFiles :: [(FilePath, FilePath)] -> IO ()
renameFiles = mconcat . fmap (uncurry Dir.renameFile)
