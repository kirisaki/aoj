import Data.Vector.Unboxed as V hiding (forM_)
import Control.Monad
import Data.List as L
import Data.Functor

main = do
  n <- read <$> getLine
  xs <- V.fromList . L.map read  . words <$> getLine :: IO (Vector Int)
  let res = solve [1..n-1] xs
  forM_ res $ \l ->
    putStrLn . L.intercalate " " . L.map show $ V.toList l

solve :: [Int] -> Vector Int -> [Vector Int]
solve [] xs = [xs]
solve (i:is) xs =
  let
    buf = xs ! i
    j = i - 1
    upd = ins j xs [] buf
  in
    xs : solve is (xs // upd)

ins :: Int -> Vector Int -> [(Int, Int)] -> Int -> [(Int, Int)]
ins j xs us buf
  | j >= 0 && xs ! j > buf = ins (j - 1) (xs // [(j+1, xs ! j)]) ((j+1, xs ! j) : us) buf
  | otherwise = (j+1, buf) : us
