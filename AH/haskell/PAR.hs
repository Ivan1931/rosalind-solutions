module PAR (doProblem) where

import System.Environment

par :: [Int] -> [Int]
par [] = []
par ls@(x:xs) = filter (<x) ls ++ filter (>=x) ls

doProblem :: ([Int] -> [Int]) -> IO ()
doProblem f = do
    args <- getArgs
    stuff <- fmap ((concatMap words) . tail . lines) (readFile (args !! 0))
    let numbers = map (\a -> read a :: Int) stuff
        ls = f numbers
        in do mapM_ (putStr . (\ a -> ' ':(show a))) ls
              putStr "\n" 

main = doProblem par
