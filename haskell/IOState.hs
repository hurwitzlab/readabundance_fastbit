import System.IO
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
 
go [] = get
go (x:xs) = do
  prev <- get
  let [n, v] = take 2 $ map (\x -> read x :: Int) $ words x
  let (lastN, lastV) = last prev
  let nextN = succ(lastN)
  if (n > nextN)
    then do put $ prev ++ [(nextN, 0)]
            go (x:xs)
    else do put $ prev ++ [(n,v)]
            go xs

main = do
  contents1 <- readFile "in1" 
  contents2 <- readFile "in2" 

  let d1 = evalState (go $ lines contents1) [(0,0)]
  let d2 = evalState (go $ lines contents2) [(0,0)]



--go fh = do
--  isEOF <- hIsEOF fh
--  prev <- get
--  if isEOF 
--    then return prev
--    else do
--      line <- liftIO $ hGetLine fh
--      let [n, v] = take 2 $ map (\x -> read x :: Int) $ words line
--      let (lastN, lastV) = last prev
--      let nextN = succ(lastN)
--      if (n > nextN)
--        then do put $ prev ++ [(nextN, 0)]
--                go fh
--        else do put $ prev ++ [(n,v)]
--                go fh
--
--main = do
--  fh <- openFile "in" ReadMode
--
--  print $ evalStateT (go fh) [(0,0)]
