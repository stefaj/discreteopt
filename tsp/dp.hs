module Main where
import Data.List
import Data.Array
import qualified Data.MemoCombinators as Mem
import Data.MemoCombinators.Class (memoize)
import System.Environment

type City = (Float,Float)

(///) (x:xs) (0,v) = v:xs
(///) (x:xs) (ind,v) = x : (///) xs (ind-1,v)     

removeFromList v [] = []
removeFromList v (x:xs) = if v == x then xs else x:(removeFromList v xs)

euclidianD (x1,y1) (x2,y2) = sqrt ((x1-x2)**2 + (y1-y2)**2)

tspdp cities n citySet = m n citySet where
  startingCity = cities !! 0
  m = Mem.integral m'
      where
        m' 1 s
          | s == [startingCity] = 0
          | otherwise = 99999999999
        m' j s = let 
                    s' = take j cities -- filter (elem startingCity) $ map (take j) $ permutations cities
                in  minimum [m (j-1) (removeFromList k s') + (euclidianD k (cities !! j)) | k <- s', k /= (cities !! j)]

lineToCity :: String -> City
lineToCity l = let [x,y] = map read (words l) in (x,y) 


argOrInput n = do
  args <- getArgs
  if length args < 1 then
    getLine
  else
    return $ args !! n

main = do
  contents <- argOrInput 0 >>= readFile
  --its <- argOrInput 1 >>= (return . read)
  let (h:c) = lines contents
  let n = (read h) :: Int
  let cities = map lineToCity c
  --putStrLn $ show (its + 1)
  let d = tspdp cities (n-1) cities
  putStrLn $ show d
  --let ff = totalDistFF cities
  --evolved <- evolver ff its n cities
  --let ans = head $ sortBy (\a b -> totalDist' a cities `compare` (totalDist' b cities)) evolved
  --let dist = totalDist' ans cities
  --putStrLn $ show dist ++ " 0"
  --putStrLn $ unwords $ map show ans
  putStrLn $ show cities