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


crossoverPop ff cross crossRate pop = aux pop
	where
	fittedPop = map (\a -> (a, ff a)) pop
	getSub _ 0 = return []
	getSub fpop gen = do
		let g 
		indiv <- rouletteSelection fpop 
	
	
		aux :: Population -> IO Population
	aux [] = return $ []
	aux (p:ps) = do
		r <- randomRIO (0,1)
		if r < mutateRate then do
								m <- mutator p
								aux ps >>= return . (m:)
			else (aux ps) >>= return . (p:)





rouletteSelection :: [(Chromosome, Float)] -> StdGen -> Chromosome
rouletteSelection xs g = R.evalRand (R.fromList $ map (\(a,b) -> (a, toRational b)) xs) g

--evolve n pop

--startEvolution popSize its crossRate mutateRate



--r <- randomRIO (0,1)
--				let cg = r*g1 + (r-1)*g2
-- return $ if cg `elem` c 
--					then cg:(helper (removeFromList cg c) g1s g2s) 
--					else c1:(helper cs g1s g2s)


toIndexedCities vs cities = map (\a -> fromJust $ a `elemIndex` cities) vs

lineToCity :: String -> City
lineToCity l = let [x,y] = map read (words l) in (x,y) 

main = do
	contents <- argOrInput 0 >>= readFile
	its <- argOrInput 1 >>= (return . read)
	let (h:c) = lines contents
	let n = (read h) :: Int
	let cities = map lineToCity c
	putStrLn $ show $ (its `mod` 2)
	--let initial = toIndexedCities (greedySearch' cities) cities
	--let action = bestPermutation cities
	--ans <- iterate' action its initial
	--let dist = totalDist' ans cities
	--putStrLn $ show dist ++ " 0"
	--putStrLn $ unwords $ map show ans
	putStrLn "awe"
