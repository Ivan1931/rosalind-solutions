import qualified Data.ByteString.Char8 as C
import qualified Data.Sequence as S
import Data.Foldable (toList)
import Data.Maybe (fromJust)
import System.Environment (getArgs)

partitionByElem :: Ord a => Int -> S.Seq a -> (S.Seq a, S.Seq a, S.Seq a)
partitionByElem i xs = (S.filter (<elem) xs, S.filter (==elem) xs, S.filter (>elem) xs)
    where elem = xs `S.index` i

dec :: Num a => a -> a
dec n = n - 1

lastSeq :: S.Seq a -> a
lastSeq xs = S.index xs (dec $ S.length xs)

findNthSmallest :: Ord a => S.Seq a -> Int -> Maybe a
findNthSmallest ws n
    | S.length ws < n = Nothing
    | otherwise = 
            let (less, eq, greater) = partitionByElem 0 ws
                lenLess  = S.length less
                lenEq    = S.length eq
                leqLen = lenLess + lenEq
                in case compare leqLen n of
                      EQ -> if lenEq /= 0 
                              then Just $ lastSeq eq
                              else Just $ lastSeq less
                      GT -> if lenLess == n 
                              then Just $ lastSeq less 
                              else if lenLess /= leqLen && lenLess < n && n < leqLen 
                                     then Just $ lastSeq eq 
                              else findNthSmallest less n
                      LT -> findNthSmallest greater (n - leqLen) -- We search for the smaller element

main = do
    (fileName:_) <- getArgs
    fileContents <- fmap (C.split '\n') $ C.readFile fileName
    let k = fst . fromJust . C.readInt $ fileContents !! 2
        a = fileContents !! 1
        numbers = (map $ fst . fromJust . C.readInt) $ (C.split ' ') a
        in putStrLn . show . fromJust $ findNthSmallest (S.fromList numbers) k
