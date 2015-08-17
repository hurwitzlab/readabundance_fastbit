{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.GroupWith (groupWith)
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import qualified Data.Map.Lazy as DM
import qualified Data.Map as Map
import           Data.List (intercalate, sort, sortBy)
import           Data.Monoid
import           Options.Applicative
import qualified Options.Applicative.Builder as OAB
import           System.Directory 
import           System.FilePath.Find 
import           System.FilePath.Posix 
                 (joinPath, takeBaseName, takeDirectory, splitFileName)
import           System.Exit

-- # --------------------------------------------------
data Options = Options {
    optInDir      :: String
  , optOutDir     :: String
  --, optTargetFile :: String
} deriving (Show)

-- # --------------------------------------------------
main :: IO ()
main = do
  execParser opts >>= runWithOptions where 
    opts   = info parser mempty
    parser = Options 
      <$> strOption       (short 'i' <> long "in"    )
      <*> strOption       (short 'o' <> long "out"   )

      -- <*> OAB.option auto (short 't' <> long "target" <> value "")

-- # --------------------------------------------------
runWithOptions opts = do
  let inDir = optInDir opts
  inDirExists <- doesDirectoryExist inDir
  when (not inDirExists) (die $ "Bad directory name: " ++ (show inDir))

  let outDir = optOutDir opts
  outDirExists <- doesDirectoryExist outDir
  when (not outDirExists) (createDirectoryIfMissing True outDir)

  files <- find always (fileType ==? RegularFile) inDir
  when (length files == 0) (die $ "Found no files in dir " ++ (show inDir))

  let fileMap = groupWith takeBaseName files
  putStrLn $ "File map = " ++ show fileMap

  let allFileNames = sort $ DM.keys fileMap
  putStrLn $ "All files = " ++ intercalate ", " allFileNames

  let fileNum = DM.fromList $ zip (sort $ DM.keys fileMap) [1..]
  putStrLn $ "fileNum = " ++ show fileNum

  putStrLn $ "Processing " ++ (show $ Map.size fileMap) ++ " groups of " ++
    (show $ length files) ++ " files from " ++ (show inDir)

  let numKeys = length $ DM.keys fileMap

  let dirName = takeBaseName . takeDirectory

  forM_ allFileNames (\fileName -> do
    case (DM.lookup fileName fileMap) of
      Nothing      -> putStrLn $ "No files for " ++ fileName
      Just fileSet -> do
        putStrLn $ "Modes for " ++ fileName
        let sortedFiles = sortBy (\a b -> dirName a `compare` dirName b) fileSet
        let dirNames = map dirName sortedFiles
        putStrLn $ "dirnames = " ++ intercalate ", " dirNames
        putStrLn $ "sortedFile = " ++ intercalate ", "  sortedFiles
        modes <- mapM parseFile sortedFiles
        let z = DM.fromList $ zip dirNames modes
        let y = DM.insert fileName (repeat 1) z
        putStrLn $ "modes = " ++ show y
        forM_ (sort $ DM.keys fileMap (\dirName ->
          case DM.lookup z dirName of
            Nothing -> return ()
            Just fileModes -> 
          )
        --modes <- mapM parseFile sortedFiles
        --let maxCount = maximum $ map length modes
        --putStrLn $ "Max = " ++ show maxCount
        --print modes
    )

  --modes <- mapM parseFile files
  --mapM_ print modes
  putStrLn "OK"

-- # --------------------------------------------------
parseReadMode = do
  readId <- decimal
  skipSpace 
  modeVal <- decimal
  return (readId, modeVal)

-- # --------------------------------------------------
fileParser = many $ parseReadMode <* endOfLine

-- # --------------------------------------------------
fillIn = go 1 where
  go _ [] = []
  go i list@((j,k):l) = case compare i j of
    EQ -> k : go (i+1) l
    _  -> 0 : go (i+1) list

-- # --------------------------------------------------
fillIn' = go 1 where
  go _ [] = []
  go i list@((j,k):l) = case compare i j of
    EQ -> (j,k) : go (i+1) l
    _  -> (i,0) : go (i+1) list

-- # --------------------------------------------------
parseFile file = do
  b <- B.readFile file
  case (parseOnly fileParser b) of
    Right modes -> return $ fillIn modes
    Left err    -> do
      putStrLn $ "Error parsing: " ++ err
      return []

-- # --------------------------------------------------
die msg = do
  putStrLn msg
  exitWith (ExitFailure 1)

