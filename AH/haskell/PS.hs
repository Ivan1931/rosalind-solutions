import Data.List
import Data.Maybe
import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

ps :: Int -> [Int] -> [Int]
ps n = (take n) . sort

main = do
    (fileName:_) <- getArgs
    contents <- fmap (tail . (C.split '\n')) $ B.readFile fileName
    let numbers = (map $ fst . fromJust . C.readInt) . (C.split ' ') $ head contents
        takeAmount = fst . fromJust $ C.readInt (contents !! 1)
        in do
          mapM_ (putStr . (++" ") . show) $ ps takeAmount numbers
          putStrLn ""
