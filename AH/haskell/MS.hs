import Data.List
import Data.Maybe
import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

merge :: Ord a => [a] -> [a] -> [a]
merge xs []         = xs
merge [] ys         = ys 
merge (x:xs) (y:ys) = if x < y
                        then x : merge xs (y:ys)
                        else y : merge (x:xs) ys

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort (x:[]) = [x]
mergeSort xs = merge (mergeSort ys) (mergeSort zs)
    where (ys, zs) = splitAt ((length xs) `div` 2) xs

main = do
    (fileName:_) <- getArgs
    contents <- fmap ((!! 1) . C.split '\n') $ B.readFile fileName
    let numbers = (map $ fst . fromJust . C.readInt) $ (C.split ' ') contents
        in mapM_ (putStr . (++ " ") . show) $ mergeSort numbers
