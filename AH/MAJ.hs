import System.Environment
import Data.List
import Data.Functor

majorityElement :: (Eq a, Ord a) => [a] -> Maybe (a, Int)
majorityElement xs = (find (\(a, len) -> (len>halfLen))) . (map (\ as@(a:_) -> (a, length as))) . group . sort $ xs
  where halfLen = length xs `div` 2

main :: IO ()
main = do
    fileName <- head <$> getArgs
    fileContents <- (tail . lines) <$> readFile fileName
    let toInt = (\ a -> read a :: Int)
        lineToNumberArray = (map toInt) . words
        toEvaluate = map lineToNumberArray fileContents
        majorityElements = map majorityElement toEvaluate
        stringify (Just (a, len)) = show a
        stringify Nothing = "-1"
        output = concat $ intersperse " " $ map stringify majorityElements
    putStrLn output
