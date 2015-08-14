{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import qualified Data.ByteString as B

parseReadMode = do
  readId <- decimal
  char '\t'
  modeVal <- decimal
  return (readId, modeVal)

fileParser = many $ parseReadMode <* endOfLine

fillIn = go 1 where
  go i [] = []
  go i list@((j,k):l) = case compare i j of
    EQ -> (j,k) : (go (j + 1) l)
    _  -> (i,0) : (go (i + 1) list)

parseFile file = do
  b <- B.readFile file
  case (parseOnly fileParser b) of
    Right modes -> return $ fillIn modes
    Left err    -> do
      putStrLn $ "Error parsing: " ++ err
      return []

main :: IO ()
main = do
  let files = ["test.fa", "test2.fa"]
  modes <- mapM parseFile files
  print modes
  putStrLn "OK"
