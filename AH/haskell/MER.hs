import System.Environment
import Data.List
import Control.Monad
import qualified Data.ByteString.Lazy as B

mergeSorted :: Ord a => [a] -> [a] -> [a]
mergeSorted p q = if length q > length p then merge [] q p else merge [] p q
    where merge as [] [] = as
          merge as (b:[]) (c:cs) = as ++ (orderMin b c) ++ cs
          merge as ds@(b:bs) es@(c:cs) = if b < c then merge (as ++ [b]) bs es else merge (as ++ [c]) ds cs
          orderMin a b = if a > b then [b, a] else [a, b]

main :: IO ()
main = 
  let wordToInt = \ a -> read a :: Int
      sentenceToInts = (map wordToInt) . words
  in do 
      fileName <- fmap head getArgs
      fileContents <- fmap lines (B.readFile fileName)
      let toMerge = map sentenceToInts [fileContents !! 1, fileContents !! 3]
          results = mergeSorted (head toMerge) (last toMerge)
      mapM_ (\ a -> (putStr . show) a >> putStr " ") results
