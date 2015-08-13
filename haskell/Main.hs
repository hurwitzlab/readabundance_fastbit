module Main where

import Control.Monad (when)
import Control.GroupWith (groupWith)
import Control.Monad.Trans.State
import Data.List (sort)
import qualified Data.Map.Lazy as DM
import qualified Data.Map as Map
import System.Directory (doesDirectoryExist, createDirectory)
import System.FilePath.Posix (joinPath, takeBaseName, splitFileName)
import System.Environment
import System.FilePath.Find
import System.Exit

main :: IO ()
main = do
  let inDir = "/Users/kyclark/work/readabundance_fastbit/t/data"
  let outDir = "out"

  inDirExists <- doesDirectoryExist inDir
  when (not inDirExists)
    (exitWithError $ "Input directory (" ++ inDir ++ ") does not exist")

  outDirExists <- doesDirectoryExist outDir
  when (not outDirExists) (createDirectory outDir) 

  files <- find always (fileType ==? RegularFile) inDir
  let numFiles = length files

  when (numFiles == 0)
    (exitWithError $ "Found no files in " ++ inDir)

  putStrLn $ "Found " ++ (show $ numFiles) ++ " files"

  --mapM_ putStrLn files

  let fileMap = groupWith takeBaseName files
  print fileMap

  mapM_ process $ DM.toList fileMap

-- | --------------------------------------------------
process :: (String, [FilePath]) -> IO ()
process (fileName, fileLocs) = do
  putStrLn $ "fileName = " ++ fileName
  --print fileLocs
  fileContents <- mapM readFile fileLocs
  let allLines = zip (repeat 0) (map lines fileContents)
  print allLines
--  runStateT go allLines
--
----go :: State Int IO ()
--go = do
   

-- | --------------------------------------------------
exitWithError :: String -> IO a
exitWithError err = do putStrLn err
                       exitWith (ExitFailure 1)
