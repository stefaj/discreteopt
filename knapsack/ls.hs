module Main where
import Data.List (sortBy)
import Data.Array
import qualified Data.MemoCombinators as Mem
import System.Environment
import Control.Monad

rotate xs 0 = xs
rotate [] _ = []
rotate (x:xs) 1 = xs ++ [x]
rotate (x:xs) n = rotate (xs ++ [x]) (n-1)

binaryPermutations n = map (rotate ((take (n-1) $ repeat 0) ++ [1])) [0..n-1]


xor a b = if a == b then 0 else 1

xorArray [] _  = []
xorArray (x:xs) (y:ys) = (x `xor` y):(xorArray xs ys)

listPermutations arr = map (xorArray arr) $ binaryPermutations (length arr)



bestPermutations arr values weights k = let 
										ans =  reverse $ sortBy (\x y -> (listMult x values) `compare` (listMult y values)) 
											 		[x | x <- listPermutations arr, listMult x weights <= k]
										helper (x:xs) = if x==0 then 0:helper xs else 0:xs
										in if ans == [] then [helper arr] else ans



iterate' arr values weights k 0 = let p = head $ bestPermutations arr values weights k
	in if listMult p values > listMult arr values then p else arr 
iterate' arr values weights k its = iterate' (head $ bestPermutations arr values weights k) values weights k (its - 1)



maximumArrByValue arrs values = reverse $ sortBy (\x y -> (listMult x values) `compare` (listMult y values)) arrs 



diversification values weights k its n = head $ maximumArrByValue arrs values
	where  
		arrs = map (it2 its k weights values) [take n $ repeat 0, take n $ concat $ repeat [0,1], take n $ concat $ repeat [1,0], take n $ repeat 1]
		it2 its k weights values arr = iterate' arr values weights k its		 




listMult _ [] = 0
listMult [] _ = 0
listMult (x:xs) (y:ys) = x*y + listMult xs ys

lineToItem :: String -> (Int, Int)
lineToItem line = let [c1,c2] = words line in (read c1, read c2)

main = do
  content <- readFile "knapdata/ks_4_0"
  let (h:c) = lines content
  let (n,k) = lineToItem h
  let items = map lineToItem c
  let values = map fst items
  let weights = map snd items
  let ans = diversification values weights k 10 n
  putStrLn $ unwords $ map show ans