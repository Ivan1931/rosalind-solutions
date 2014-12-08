import System.Environment
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

makeLookupTable :: [Int] -> M.Map Int Int
makeLookupTable = M.fromList . (flip L.zip) [0..]

orderThree :: (Int, Int, Int) -> (Int, Int, Int)
orderThree (a, b, c) = firstThree $ L.sort [a, b, c]
    where firstThree xs = (xs !! 0, xs !! 1, xs !! 2)

threeSum :: [Int] -> Maybe (Int, Int, Int)
threeSum [] = Nothing
threeSum xs = threeSumIter (0,0) (xs, xs)
  where lookupTable = makeLookupTable xs
        threeSumIter (a, b) xss = case xss of
                                    ((y:[]), []) -> Nothing
                                    ((y:ys), []) -> threeSumIter (a + 1, 0) (ys, xs)
                                    (yss@(y:ys), (z:zs)) -> if a /= b then
                                                              case M.lookup (-(y + z)) lookupTable of
                                                                Just i -> Just . orderThree $ (i, a, b)
                                                                _      -> threeSumIter (a, b + 1) (yss, zs)
                                                            else threeSumIter (a, b + 1) (yss, zs)

bwords = C.split ' '
blines = C.split '\n'

main = do
    (arg:_) <- getArgs
    xs <- fmap (L.map bwords . L.init . L.tail . blines) $ B.readFile arg
    let nums = L.map (\ a -> L.map (fst . fromJust . C.readInt) a) xs
        results = L.map threeSum nums
        in forM_ results $ do \ a -> case a of
                                       Nothing -> Prelude.putStrLn "-1"
                                       Just (a, b, c) -> Prelude.putStrLn $ (show (a + 1)) ++ " " ++ (show (b + 1)) ++ " " ++ (show (c + 1))
