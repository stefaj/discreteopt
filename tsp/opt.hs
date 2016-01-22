import Data.List
import System.Environment
import Data.Maybe (fromJust)
import Control.Monad
import System.Random
import Data.Array

type City = (Float,Float)

euclidianD (x1,y1) (x2,y2) = sqrt ((x1-x2)**2 + (y1-y2)**2)

argOrInput n = do
	args <- getArgs
	if length args < 1 then
		getLine
	else
		return $ args !! n

(///) (x:xs) (0,v) = v:xs
(///) (x:xs) (ind,v) = x : (///) xs (ind-1,v)  		


removeFromList v [] = []
removeFromList v (x:xs) = if v == x then xs else x:(removeFromList v xs)


buildInitial cites vs 0 = vs
buildInitial cities vs n = buildInitial cities ((cities !! n):cities) (n-1)


getLines [] = []
getLines [v] = []
getLines (v1:v2:vs) = (v1,v2):getLines (v2:vs) 

intersection :: (City,City) -> (City,City) -> Bool
intersection ((ax,ay),(bx,by)) ((cx,cy),(dx,dy))
	| h > 0 && h < 1 = True
	| otherwise = False
	where 
		cross (vx,vy) (wx,wy) = vx*wy - vy*wx
		dot (vx,vy) (wx,wy) = vx*wx + vy*wy
		(/-) (x1,y1) (x2,y2) = (x1-x2,y1-y2)
		a = (ax,ay)
		c = (cx,cy)
		(ex,ey) = (bx-ax, by-ay)
		f = (dx-cx,dy-cy)
		p = (-ey,ex)
		h = ( (a /- c) `dot` p ) / (f `dot` p)

-- this function is really slow due to replicateM
intersectionCount :: [City] -> [Int] -> Int
intersectionCount cities vs' = 	let 
									vs = map (\a -> cities !! a) vs' 
									l = getLines vs
									l2 = replicateM 2 l
								in 
									length $ filter (\[a,b] -> intersection a b) l2


--[0,3,1,5] 
swap vs i1 i2 = let
	v1' = vs !! i1
	v2' = vs !! i2 
	in (vs /// (i1, v2')) /// (i2,v1')


swapA vs i1 i2 = let
	v1' = vs ! i1
	v2' = vs ! i2
	in vs // [(i1,v2'),(i2,v1')]


randomValList :: [a] -> IO a
randomValList xs = do
	let n = length xs
	i <- randomRIO (0,n-1)
	return $ xs !! i


-- bestPermutation :: [City] -> [Int] -> IO [Int]
randomRIONot a b vs = do
	v <- randomRIO (a,b)
	if v `elem` vs then randomRIO a b vs else return v 


--threeOpt :: [City] -> [Int] -> IO [Int]
threeOpt cities vs = do
	let n = length vs
	a <- randomRIONot (0,n-1) []
	b <- randomRIONot (0,n-1) [a]
	c <- randomRIONot (0,n-1) [a,b]
	let opt2 = swapA vs a b
	let opt31 = swapA opt2 b c
	let opt32 = swapA opt2 a c
	let ab = euclidianD (cities ! a) (cities ! b)
	let bc = euclidianD (cities ! b) (cities ! c)
	let ac = euclidianD (cities ! a) (cities ! c)
	return $ head $ sortBy (\a b -> totalDist' a cities `compare` (totalDist' b cities)) [vs, opt2,opt31,opt32]
	

--intersections cities vs = 

-- local search ideas
-- 1 - minimize number of intersections between edges
-- 2 - create permutation, swap 2 vertices to reduce total distance
-- 3 - combine 1 and 2

iterate' :: ([Int] -> IO [Int]) -> Int -> [Int] -> IO [Int]
iterate' action 0 inp = action inp
iterate' action n inp = do
	k <- action inp
	iterate' action (n-1) k




totalDist [] = 0

totalDist (v:[]) = 0 
totalDist (v1:v2:vs) = euclidianD v1 v2 + totalDist (v2:vs) 
totalDist' vs' cities = let vs = map (\a -> cities !! a) vs' in
	(totalDist vs) + (euclidianD (head vs) (last vs))


-- greedy stuff
greedySearch [] = []
greedySearch [c] = []
greedySearch (c:cs) = closestCity : (greedySearch (closestCity:cs'))
	where 
		closestCity = head $ sortBy (\c1 c2 -> (euclidianD c1 c) `compare` (euclidianD c2 c)) cs
		cs' = removeFromList closestCity cs

greedySearch' (c:cs) = c:(greedySearch (c:cs))

toIndexedCities :: [City] -> [City] -> [Int]
toIndexedCities vs cities = map (\a -> fromJust $ a `elemIndex` cities) vs



lineToCity :: String -> City
lineToCity l = let [x,y] = map read (words l) in (x,y) 

main = do
	contents <- argOrInput 0 >>= readFile
	its <- argOrInput 1 >>= (return . read)
	let (h:c) = lines contents
	let n = (read h) :: Int
	let cities = map lineToCity c
	let initial = toIndexedCities (greedySearch' cities) cities
	
	--putStrLn $ (show dist) ++ " 0"
	
	--greedy tsp51 is 506
	--let ans = initial
	let action = threeOpt cities
	ans <- iterate' action its initial
	
	let dist = totalDist' ans cities
	putStrLn $ show dist ++ " 0"
	
	putStrLn $ unwords $ map show ans
	-- putStrLn $ show $ (its + 1)


-- some optimizations for the greedy is to try different methods in order to visit cities. try taking longest path first. 


-- give each city a value, the total distance to all nearby cities. call this cluster value. visit big city with low cluster value. visit nearby cities. visit next big city. repeat.