module Main where
import Data.List (foldl')
import Data.Array
import qualified Data.MemoCombinators as Mem
import Data.MemoCombinators.Class (memoize)
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

knapsack2 items n k = m n k where
  ind = indices items
  m = Mem.integral m'
      where
        m' 0 w = (0, [])
        m' i w =
            let (vi,wi) = items ! (i-1) in
              if wi > w || wi < 0 || vi < 0 then m (i-1) w
                else
                  let (a,b) = ((m (i-1) w), (m (i-1) (w-wi))) in
                  if fst a > fst b + vi then a
                    else let (n,s) = b in (n+vi,(i,w):s)
        --m' i w = m (i-1) w 


traceback arr items 0 _ = []

traceback arr items i k = 
  if ((i, k) `elem` arr) 
    then i:(traceback arr items (i-1) (k-(snd $ items ! (i-1))))
    else traceback arr items (i-1) k


normalizeOutput arr items n k = 
  let items' = map (\a -> a - 1) $ traceback arr items n k 
  in map (\a -> if a `elem` items' then 1 else 0) [0..n-1] 


lineToItem :: String -> (Int, Int)
lineToItem line = let [c1,c2] = words line in (read c1, read c2)

printOutput l = unwords $ map show l

getScaleFactor items = minimum [1, 10 ^ ((truncate $ log $ fromIntegral $ minimum $ map snd items)-1)]

main = do
  args <- getArgs
  content <- if length args > 0 then readFile (args !! 0) else getLine >>= readFile
  let (h:c) = lines content
  let (n,k) = lineToItem h
  let items = map lineToItem c
  let sf = getScaleFactor items
  -- let itemsMod = map (\(a,b) -> (a, b `div` sf) ) items
  -- let kMod = k `div` sf
  let itemArr = array (0,n-1) [(i,(v,w)) | i <- [0..n-1], let item = items !! i, let (v,w) = if snd item < k then item else (-1,-1)]
  let (opt, arr) = knapsack2 itemArr n k
  let t = traceback arr itemArr n k
  let output = normalizeOutput arr itemArr n k
  let value = sum $ map fst $ map (\a -> items !! (a-1)) t
  putStrLn $ (show value) ++ " 1"
  putStrLn $ printOutput output
  -- putStrLn $ show itemArr