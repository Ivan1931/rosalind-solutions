import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M
import Data.List
import Data.Maybe
import System.Environment

inversions :: [Int] -> Int
inversions [] = 0
inversions (x:xs) = (length $ filter (<x) xs) + inversions xs

inversions' :: [Int] -> Int
inversions' xs = foldl' reducer 0 xs
    where makeLookup = M.fromList . ((flip zip) [0..])
          unsortedLookup = makeLookup xs
          sortedLookup = makeLookup $ sort xs
          reducer acc a = let dist = sortedLookup M.! a - unsortedLookup M.! a
                              in if dist > 0 
                                   then acc + dist
                                   else acc

main = do
    (fileName:_) <- getArgs
    contents <- fmap ((!! 1) . C.split '\n') $ B.readFile fileName
    let numbers = (map $ fst . fromJust . C.readInt) $ (C.split ' ') contents
        in putStrLn . show . inversions' $ numbers
