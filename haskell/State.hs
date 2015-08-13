import System.IO
import Control.Monad.Trans.State
 
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
  let list = ["1\t4", "2\t1", "5\t2"]
  print $ evalState (go list) [(0,0)]
