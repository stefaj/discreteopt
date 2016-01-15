import Data.List
import System.Environment
import Data.Maybe (fromJust)

type City = (Float,Float)

euclidianD (x1,y1) (x2,y2) = sqrt ((x1-x2)**2 + (y1-y2)**2)

argOrInput n = do
	args <- getArgs
	if length args < 1 then
		getLine
	else
		return $ args !! n


removeFromList v [] = []
removeFromList v (x:xs) = if v == x then xs else x:(removeFromList v xs)


greedySearch [] = []
greedySearch [c] = []
greedySearch (c:cs) = closestCity : (greedySearch (closestCity:cs'))
	where 
		closestCity = head $ sortBy (\c1 c2 -> (euclidianD c1 c) `compare` (euclidianD c2 c)) cs
		cs' = removeFromList closestCity cs

greedySearch' (c:cs) = c:(greedySearch (c:cs))

totalDist [] = 0
totalDist (v:[]) = 0 
totalDist (v1:v2:vs) = euclidianD v1 v2 + totalDist (v2:vs) 
totalDist' vs = (totalDist vs) + (euclidianD (head vs) (last vs))

toIndexedCities vs cities = map (\a -> fromJust $ a `elemIndex` cities) vs

lineToCity :: String -> City
lineToCity l = let [x,y] = map read (words l) in (x,y) 

main = do
	contents <- argOrInput 0 >>= readFile
	let (h:c) = lines contents
	let n = read h :: Float
	let cities = map lineToCity c
	let ans = greedySearch' cities
	let dist = totalDist' ans
	putStrLn $ (show dist) ++ " 0"
	putStrLn $ unwords $ map show (toIndexedCities ans cities)
	--putStrLn $ show ans