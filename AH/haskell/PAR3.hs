import System.Environment
import Data.List
import PAR

par3 :: [Int] -> [Int]
par3 ls@(x:xs) = filter (<x) ls ++ filter (==x) ls ++ filter (>x) ls

main = doProblem par3
