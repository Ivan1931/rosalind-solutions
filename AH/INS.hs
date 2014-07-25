import System.Environment
import Control.Monad.ST
import Control.Monad
import Data.STRef
import qualified Data.Vector as V


insertionSort :: Ord a => V.Vector a -> (Int, V.Vector a)
insertionSort as = runST $
  let n = V.length as
      while 0 _ _ = return ()
      while k ss swaps = do
        a <- readSTRef (ss V.! k)
        b <- readSTRef (ss V.! (k - 1))
        if a < b then do --Swap elements
          writeSTRef (ss V.! (k - 1)) a
          writeSTRef (ss V.! k) b
          modifySTRef swaps (+1)
          while (k - 1) ss swaps
        else return ()
  in do
    ss <- V.mapM newSTRef as
    swaps <- newSTRef 0
    forM_ [1..n - 1] $ \ i -> do
      while i ss swaps
    swaps <- readSTRef swaps
    ss <- V.mapM readSTRef ss
    return (swaps, ss)

main :: IO () 
main = 
  let wordToInt = \ a -> read a :: Int
      lineToInts = (map wordToInt) . words
  in do
      fileName <- fmap head getArgs
      (swaps, nums) <- fmap (insertionSort . V.fromList . lineToInts . last . lines) (readFile fileName)
      V.mapM_ (\ num -> (putStr . show) num >> putStr " ")  nums
      putStrLn ("\n\n" ++ (show swaps))
