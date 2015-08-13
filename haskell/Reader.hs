module Reader where

{-# LANGUAGE OverloadedStrings #-} 

import Turtle      
import System.IO
-- import Data.List (fold')

main :: IO ()
main = do
  c <- readFile "/Users/kyclark/work/readabundance_fastbit/t/data/C/B"
  --mapM_ putStrLn (lines c)
  foldl (\lastN line -> 
    let [n, v] = take 2 $ words line
    let thisN = read n :: Int
    let diff = thisN - lastN
     
    ) (0,0) (lines c)
