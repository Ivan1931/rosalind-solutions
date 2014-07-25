import Data.List

mergeSorted :: Ord a => [a] -> [a] -> [a]
mergeSorted = merge []
 where merge cs [] [] = cs
       merge cs (a:as) (b:[]) = cs ++ (orderedTwo a b) ++ as
       merge cs (a:[]) (b:bs) = cs ++ (orderedTwo a b) ++ bs
       merge cs (a:as) list@(b:bs)
        | a < b  = merge (cs ++ [a]) as list
        | a >= b = merge (cs ++ [b, a]) as bs
       orderedTwo a b = if a > b then [b, a] else [a, b]
