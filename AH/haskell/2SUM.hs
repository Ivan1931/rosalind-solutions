import System.Environment
import qualified Data.Map as M
import qualified Data.List as L
import Control.Monad.State
import Control.Monad
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.Maybe


lookupTable = M.fromList [(i, (-1)) | i <- [(-100000 :: Int)..(100000 :: Int)]]

type LookupTable = M.Map Int Int
type MaybeTuple = Maybe (Int, Int)

twoSumIter :: [Int] -> Int -> State LookupTable MaybeTuple
twoSumIter [] _ = return Nothing
twoSumIter (x:xs) i = do env <- get
                         let n = env M.! (-x)
                             in if (n /= (-1))
                                  then return $ Just (n, i)
                                  else put (M.insert x i env) >> twoSumIter xs (i + 1)

twoSum :: [Int] -> MaybeTuple
twoSum ls = fst (runState (twoSumIter ls 1) lookupTable)

bwords = C.split ' '
blines = C.split '\n'

main = do
    (arg:_) <- getArgs
    xs <- fmap (L.map bwords . L.tail . blines) $ B.readFile arg
    let nums = L.map (\ a -> L.map (fst . fromJust . C.readInt) a) xs
        results = L.map twoSum nums
        in forM_ results $ do \ a -> case a of
                                    Nothing -> Prelude.putStrLn "-1"
                                    Just (a, b) -> Prelude.putStrLn $ (show a) ++ " " ++ (show b)
