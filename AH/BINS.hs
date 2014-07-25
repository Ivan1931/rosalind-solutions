import qualified Data.Vector as V
import qualified Data.List as L
import System.Environment
import Data.Char

binnarySearch :: Ord a => V.Vector a -> a -> Int
binnarySearch vals elem = binnarySearch' 0 (V.length vals - 1)
    where binnarySearch' low high
            | (vals V.! high) == elem = high + 1
            | (vals V.! low) == elem = low + 1
            | mid == low = (-1)
            | (vals V.! mid) < elem = binnarySearch' mid high
            | (vals V.! mid) > elem = binnarySearch' low mid
            | (vals V.! mid) == elem = mid + 1
            | otherwise = (-1)
            where mid = (high + low) `div` 2
            
            
main :: IO ()
main = do 
     fileName <- fmap head getArgs
     stuff <- fmap lines (readFile fileName)
     let lookin = V.fromList $ toNumbers (stuff !! 2)
         toInt a = (read a :: Int)
         toNumbers =  (map toInt) . words
         lookfor = toNumbers (stuff !! 3)
         results = map (binnarySearch lookin) lookfor
         resultToString =  concat . (L.intersperse " ") . (map show)
     putStrLn (resultToString results)

