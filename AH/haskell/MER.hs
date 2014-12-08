module MER (merge) where

import System.Environment
import Data.List
import Control.Monad

merge :: Ord a => [a] -> [a] -> [a]
merge rs ws = merge' rs ws []
    where merge' xs [] acc = acc ++ xs
          merge' [] ys acc = acc ++ ys
          merge' xss@(x:xs) yss@(y:ys) acc
            | y > x = merge' xs yss (acc ++ [x])
            | otherwise = merge' xss ys (acc ++ [y])
