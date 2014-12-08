import System.Environment
import qualified Data.Map as M 
import qualified Data.Vector as V
import Control.Monad.State
import Data.Maybe
import qualified Data.List as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

type Grid = V.Vector (V.Vector Int)

lessThanTwo :: Int -> Int -> Int -> Bool
lessThanTwo a b c = c < a && c < b

sortBySecond :: Ord b => [(a, b)] -> [(a, b)]
sortBySecond = L.sortBy (\ (_, a) (_, b) -> compare b a)

valueAt :: Grid -> (Int, Int) -> Maybe Int
valueAt g (x, y) = case g V.!? x of
                     Just p -> p V.!? y
                     _      -> Nothing

-- Make sure that xs is sorted
makeGrid :: [Int] -> Grid
makeGrid xs = V.fromList [V.fromList (map (+x) xs) | x <- xs]

searchOrderedGrid :: Grid -> Int -> Maybe (Int, Int)
searchOrderedGrid grid i = searchIter (0, 0)
    where searchIter xy@(x, y)
            | Just (-i) == val xy = Just (x, y) -- First we check if the current square is equal to the negation the search value
            | isNothing rightValue && isNothing diagonalValue = Nothing -- If we are at the lowest corner block and have reached this point we have found nothing
            | fromJust rightValue > (-i) && fromJust diagonalValue >= (-i) = searchIter diagonal -- 
            | fromJust rightValue >= (-i) && fromJust diagonalValue < (-i) = searchIter right -- If the diagonal square is less than our target but the right square is greater move right
            | otherwise = Nothing -- In this case both diagonal and right are less than our search square. Thus the search square cannot possibly be found and we have nowhere to go, exit
            where right    = (x + 1, y)
                  down     = (x, y + 1)
                  diagonal = (x + 1, y + 1)
                  val = valueAt grid
                  rightValue    = val right
                  downValue     = val down
                  diagonalValue = val diagonal

maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

threeSum :: [Int] -> Maybe (Int, Int, Int)
threeSum xs = case result of
                Just (a, Just (b, c)) -> Just (a, sortedMap M.! b, sortedMap M.! c)
                _                     -> Nothing
    where sortedTuples = sortBySecond $ zip [0..] xs
          sortedMap = M.fromList (zip [0..] $ map fst sortedTuples) -- Map of (a, b) where a means that index a of sorted list correspons to index b in unsorted list
          sorted = map snd sortedTuples
          grid = makeGrid sorted
          result = maybeHead $ ((filter $ isJust . snd) . (zip [0..]) . (map $ searchOrderedGrid grid)) xs

bwords = C.split ' '
blines = C.split '\n'

main = do
    (arg:_) <- getArgs
    xs <- fmap (L.map bwords . L.tail . blines) $ B.readFile arg
    let nums = L.map (\ a -> L.map (fst . fromJust . C.readInt) a) xs
        results = L.map threeSum nums
        in forM_ results $ do \ a -> case a of
                                    Nothing -> Prelude.putStrLn "-1"
                                    Just (a, b, c) -> Prelude.putStrLn $ (show (a + 1)) ++ " " ++ (show (b + 1)) ++ " " ++ (show (c + 1))
