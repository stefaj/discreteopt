import Data.List
import System.Environment
import Data.Maybe (fromJust)
import Control.Monad
import System.Random
import qualified Control.Monad.Random as R

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



swap vs i1 i2 = let
	v1' = vs !! i1
	v2' = vs !! i2 
	in (vs /// (i1, v2')) /// (i2,v1')

type Chromosome = [Int]
type Population = [Chromosome]
type FitnessFunction = (Chromosome -> Float) 

crossover :: Chromosome -> Chromosome -> IO Chromosome
crossover gene1 gene2 = 
	let 
		n = length gene1
		helper [] _ _ = return $ []
		helper c@(c1:cs) (g1:g1s) (g2:g2s) = do
				r <- randomRIO (0::Int,1::Int)
				let cg = r*g1 + (1-r)*g2
				if cg `elem` c 
					then (helper (removeFromList cg c) g1s g2s) >>= return . (cg:)
					else (helper cs g1s g2s) >>= return . (c1:) 
	in helper [0..n-1] gene1 gene2

mutation :: Chromosome -> IO Chromosome
mutation gene =	do
	c <- randomRIO (0,max 1 (n `div` 5))
	p c (return gene)
	where
		p :: Int -> IO Chromosome -> IO Chromosome
		p 0 g = g
		p n g = p (n-1) (g >>= permute) 
		n = length gene
		permute g = do
			i <- randomRIO (0,n-1)
			j <- randomRIO (0,n-1)
			return $ swap g i j


mutatePop mutator mutateRate pop = aux pop
	where
	aux :: Population -> IO Population
	aux [] = return $ []
	aux (p:ps) = do
		r <- randomRIO (0,1)
		if r < mutateRate then do
								m <- mutator p
								aux ps >>= return . (m:)
			else (aux ps) >>= return . (p:)

crossoverPop :: (Chromosome -> Chromosome -> IO Chromosome) -> Float -> [(Chromosome,Float)] -> IO Population
crossoverPop cross crossRate fpop = do
		gen <- newStdGen
		let count = 2 * (truncate $ crossRate * fromIntegral (length fpop))
		let sub = getSubSelection fpop gen count
		--putStrLn $ show fpop
	 	aux sub
	where
		
		aux :: Population -> IO Population
		aux [] = return $ []
		aux [p] = return $ []
		aux (p1:p2:ps) = do
			child <- cross p1 p2
			aux (p2:ps) >>= return . (child:)


fittedPop :: FitnessFunction -> Population -> [(Chromosome,Float)]
fittedPop fitnessFunction population = sortBy (\(a,b) (c,d) -> d `compare` b) $ map (\a -> (a, fitnessFunction a)) population

getSubSelection :: [(Chromosome,Float)] -> StdGen -> Int -> Population
getSubSelection _ _ 0 = []
getSubSelection fpop gen n = let 
		(_, gen') = next gen
		indiv = rouletteSelection fpop gen
	in indiv:(getSubSelection fpop gen' (n-1))


evolve :: Int -> Float -> Float -> Population -> FitnessFunction -> [City] -> Int -> IO Population
evolve popCount crossRate mutateRate population fitnessFunc _ 0 = return $ population
evolve popCount crossRate mutateRate population fitnessFunc cities n = do
	let copyRate = (1-crossRate)
	let copyCount = truncate $ copyRate * fromIntegral (length population)
	let fpop = fittedPop fitnessFunc population
	gen <- newStdGen
	let copies = getSubSelection fpop gen copyCount
	crosses <- crossoverPop crossover crossRate fpop
	bperm <- bestPermutation cities $ fst $ (fpop !! 1)
	let newPop' = (take popCount $ crosses ++ copies) ++ [fst $ head $ fpop] ++ [bperm]
	--putStrLn $ show $ maximum $ map fitnessFunc newPop' 
	newPop <- mutatePop mutation mutateRate newPop'
	evolve popCount crossRate mutateRate newPop fitnessFunc cities (n-1)


rouletteSelection :: [(Chromosome, Float)] -> StdGen -> Chromosome
rouletteSelection xs g = R.evalRand (R.fromList $ map (\(a,b) -> (a, toRational b)) xs) g


evolver ff its n cities = do
	let popCount = 80
	let greedy = (greedySearch' cities)  
	let vreedy = toIndexedCities cities greedy
	let pops = vreedy:(take (popCount-1) $ permutations [0..n-1])
	evolve popCount 0.2 0.1 pops ff cities its


greedySearch :: [City] -> [City]
greedySearch [] = []
greedySearch [c] = []
greedySearch (c:cs) = closestCity : (greedySearch (closestCity:cs'))
	where 
		closestCity = head $ sortBy (\c1 c2 -> (euclidianD c1 c) `compare` (euclidianD c2 c)) cs
		cs' = removeFromList closestCity cs
greedySearch' (c:cs) = c:(greedySearch (c:cs))


totalDistFF :: [City] -> FitnessFunction 
totalDistFF cities pop = 1 / (totalDist' pop cities)




bestPermutation :: [City] -> [Int] -> IO [Int]
bestPermutation cities vs = do
	let n = length cities
	i <- randomRIO (0,n-1)
	j <- randomRIO (0,n-1)
	let k = swap vs i j
	return $ if totalDist' k cities < totalDist' vs cities then k else vs



totalDist [] = 0

totalDist (v:[]) = 0 
totalDist (v1:v2:vs) = euclidianD v1 v2 + totalDist (v2:vs) 
totalDist' vs' cities = let vs = map (\a -> cities !! a) vs' in
	(totalDist vs) + (euclidianD (head vs) (last vs))



toIndexedCities :: [City] -> [City] -> [Int]
toIndexedCities cities vs = map (\a -> fromJust $ a `elemIndex` cities) vs

lineToCity :: String -> City
lineToCity l = let [x,y] = map read (words l) in (x,y) 

main = do
	contents <- argOrInput 0 >>= readFile
	its <- argOrInput 1 >>= (return . read)
	let (h:c) = lines contents
	let n = (read h) :: Int
	let cities = map lineToCity c
	let ff = totalDistFF cities
	evolved <- evolver ff its n cities
	let ans = head $ sortBy (\a b -> totalDist' a cities `compare` (totalDist' b cities)) evolved
	--let initial = toIndexedCities (greedySearch' cities) cities
	--let action = bestPermutation cities
	--ans <- iterate' action its initial
	let dist = totalDist' ans cities
	putStrLn $ show dist ++ " 0"
	putStrLn $ unwords $ map show ans
	--putStrLn $ show ans
