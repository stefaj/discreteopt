import Data.List
import System.Environment
import Data.Maybe (fromJust)
import Control.Monad
import System.Random
import Data.Array
import Control.Monad

type City = (Float,Float)

euclidianD (x1,y1) (x2,y2) = sqrt ((x1-x2)**2 + (y1-y2)**2)
euclidianDT (c1,c2) = euclidianD c1 c2

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
randomRIONot :: Int -> Int -> [Int] -> IO Int
randomRIONot a b vs = do
	v <- randomRIO (a,b)
	if v `elem` vs then randomRIONot a b vs else return v


randomRList :: [a] -> IO a
randomRList xs = do
	i <- randomRIO (0, (length xs) - 1)
	return $ xs !! i


type IOSearchFunction = Array Int Int -> IO (Array Int Int)

averageDist cities = let edges = uPerms cities
		in (sum $ map euclidianDT edges) / (fromIntegral $ length cities)
	where
		uPerms [] = []
		uPerms (c:cs) = liftM2 (,) [c] cs ++ (uPerms cs)


heuristicSwap :: Float -> Array Int City -> IOSearchFunction
heuristicSwap avgDist cities vs = do
	let n = 1 + (snd $ bounds vs)
	i <- randomRIO (0,n-1)
	let cj = head $ [j | j <- [0..n-1], let c = cities ! j, j /= i, euclidianD c (cities ! i) < avgDist/4] ++ [0..i-1] ++ [i+1..n-1]
	return $ head $ sortBy (\a b -> totalDistA a cities `compare`  (totalDistA b cities) ) [swapA vs i cj, vs]

e = exp 1 :: Float

annealing f a s t 0 mini = return $ snd mini
annealing f a s t its (mv,mi) = do
	let n = length s
	j <- randomRIO (1,n-1)
	k <- randomRIONot 1 (n-1) [j]
	let s' = a k j s
	r <- randomRIO (0,0.9999)
	let t' = if its `mod` 20 == 0 then t*0.95 else t
	case () of _
				| (f s') < (f s) -> annealing f a s' t' (its-1) (if f s' < mv then (f s',s') else (mv,mi))
				| otherwise -> if r <= e ** ((f s - (f s') )/t)
						then annealing f a s' t' (its-1) (mv,mi)
						else annealing f a s t' (its-1) (mv,mi)




a k j s = if k > j then (take (j) s) ++ (reverse $ splice s j k) ++  (drop (k+1) s)
	else (take (k) s) ++ (reverse $ splice s k j) ++  (drop (j+1) s)
--splice xs _ (-1) = []
--splice (x:xs) 0 j = x:(splice xs 0 (j-1))
--splice (x:xs) i j = splice xs (i-1) (j-1)
--splice [] _ _ = []

splice xs i j = take (j-i+1) $ (drop i xs)


splice3 xs i j = [take (i) xs] ++ [splice xs i j] ++  [drop (j+1) xs]




annealingTsp cities vs temp its = annealing (flip totalDist' cities) a vs temp its (999999999,[])

threeOpt :: Array Int City -> IOSearchFunction
threeOpt cities vs = do
		let n = 1 + (snd $ bounds vs)
		a <- randomRIONot 1 (n-1) []
		b <- randomRIONot 1 (n-1) [a]
		c <- randomRIONot 1 (n-1) [a,b]
		let oldEges = map (\(q,w) -> (cities ! q, cities ! w)) $ [(vs ! (a-1), vs ! a),(vs ! (b-1),vs ! b)]
		let opt2 = swapA vs a b
		let newEdges = map (\(q,w) -> (cities ! q, cities ! w)) $ [(opt2 ! (a-1), opt2 ! a),(opt2 ! (b-1),opt2 ! b)]
		return $ if dist newEdges < dist oldEges then opt2 else vs
		--let opt31 = swapA opt2 b c
		--let opt32 = swapA opt2 a c

		--return $ getMaxA cities [vs,opt2,opt31,opt32]
	where
		getMaxA :: Array Int City -> [Array Int Int] -> Array Int Int
		getMaxA cities ass = head $ sortBy (\a b -> totalDistA a cities `compare` (totalDistA b cities)) ass
		getEdges :: Int -> Int -> [(City,City)]
		getEdges n i = map (\(j,k) -> (cities ! j, cities ! k))  $ filter (\(a,b) -> a>=0 && b < n) [(i-1,i),(i,i+1)]
		dist edges = sum $ map euclidianDT edges

--intersections cities vs =

-- local search ideas
-- 1 - minimize number of intersections between edges
-- 2 - create permutation, swap 2 vertices to reduce total distance
-- 3 - combine 1 and 2

--iterate' :: ([Int] -> IO [Int]) -> Int -> [Int] -> IO [Int]
iterate' action 0 inp = action inp
iterate' action n inp = do
	k <- action inp
	iterate' action (n-1) k




totalDist [] = 0

totalDist (v:[]) = 0
totalDist (v1:v2:vs) = euclidianD v1 v2 + totalDist (v2:vs)
totalDist' vs' cities = let vs = map (\a -> cities !! a) vs' in
	(totalDist vs) + (euclidianD (head vs) (last vs))
totalDistA vs cities = let
		vs' = elems vs
		cities' = elems cities
		in totalDist' vs' cities'

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
	--let cities' = array (0,n-1) [(i,cities !! i) | i <- [0..n-1]]
	let initial = toIndexedCities (greedySearch' cities) cities
	--let initial' = array (0,n-1) [(i, initial !! i) | i <- [0..n-1]]
	let avgDist = averageDist cities

	--greedy tsp51 is 506

	--let action = threeOpt cities'
	--ans <- iterate' action its initial'
	ans <- annealingTsp cities initial (avgDist*(fromIntegral n)) its

	let dist = totalDist' ans cities
	putStrLn $ show dist ++ " 0"

	putStrLn $ unwords $ map show ans
