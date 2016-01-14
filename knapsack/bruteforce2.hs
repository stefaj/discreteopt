module Main where
import Data.List (foldl')
import Data.Array
import qualified Data.MemoCombinators as Mem
import System.Environment


knapsack items wmax = m wmax
 where
    m 0 = 0
    m w = maximum $ 0:[vi + m (w - wi) | (vi, wi) <- items, wi <= w]


knapsack' items n k = m n k where
  --m :: Int -> Int -> [(Int, [(Int, Int)])] ->(Int, Int)
  m 0 w = (0, (array ((0,0), (n-1,k)) [((a,b),0) | a <- [0..n-1], b<- [0..k]] ))
  m i w
      | wi > w = m (i-1) w
      | fst (m (i-1) w) > fst (m (i-1) (w-wi)) + vi = m (i-1) w
      | otherwise =  let (n,s) = (m (i-1) (w-wi)) in (n+vi,s// [((i-1,w),1)])
    where (vi,wi) = items !! (i-1)



traceback arr items 0 _ = []
--traceback arr items i k = if arr ! (i k) == 1 then i:(traceback arr (i-1) items (k-(snd $ items !! i))) else traceback arr items (i-1) k

traceback arr items i k = 
  if (arr ! (i-1, k)) == 1 
    then i:(traceback arr items (i-1) (k-(snd $ items !! (i-1))))
    else traceback arr items (i-1) k
--traceback = map (\a -> (a-1)) traceback

normalizeOutput arr items n k = 
  let items' = map (\a -> a - 1) $ traceback arr items n k 
  in map (\a -> if a `elem` items' then 1 else 0) [0..n-1] 



lineToItem :: String -> (Int, Int)
lineToItem line = let [c1,c2] = words line in (read c1, read c2)

main = do
  content <- readFile "knapdata/ks_4_0"
  let (h:c) = lines content
  let (n,k) = lineToItem h
  let items = map lineToItem c
  let (opt, arr) = knapsack' items n k
  let output = normalizeOutput arr items n k
  putStrLn $ show output