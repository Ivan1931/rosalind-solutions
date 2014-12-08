import MER
import Data.List
import Data.Maybe
import System.Environment
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

mergeSort :: [Int] -> [Int]
mergeSort (x:[]) = [x] -- Base case, one index array is sorted
mergeSort xs = merge (mergeSort ys) (mergeSort zs)
    where (ys, zs) = splitAt ((length xs) `div` 2) xs

main = do
    (fileName:_) <- getArgs
    contents <- fmap (last . C.split '\n') $ B.readFile fileName
    let numbers = (map $ fst . fromJust . C.readInt) $ (C.split ' ') contents
        in mapM_ (putStr . (++" ") . show) $ mergeSort numbers
