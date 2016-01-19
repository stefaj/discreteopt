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

type Chromosome a = [a]
type Population a = [Chromosome a]
type FitnessFunction a = (Chromosome a -> Float) 

mutatePop :: (Chromosome a -> IO (Chromosome a)) -> Int -> Population a -> IO (Population a)
mutatePop mutator mutateRate pop = aux pop
	where
	aux [] = return $ []
	aux (p:ps) = do
		r <- randomRIO (0,1)
		if r < mutateRate then do
								m <- mutator p
								aux ps >>= return . (m:)
			else (aux ps) >>= return . (p:)

crossoverPop :: (Chromosome a -> Chromosome a -> IO (Chromosome a)) -> Float -> [(Chromosome a,Float)] -> IO (Population a)
crossoverPop cross crossRate fpop = do
		gen <- newStdGen
		let count = 2 * (truncate $ crossRate * fromIntegral (length fpop))
		let sub = getSubSelection fpop gen count
	 	aux sub
	where
		aux [] = return $ []
		aux [p] = return $ []
		aux (p1:p2:ps) = do
			child <- cross p1 p2
			aux (p2:ps) >>= return . (child:)


fittedPop :: FitnessFunction a -> Population a -> [(Chromosome a,Float)]
fittedPop fitnessFunction population = sortBy (\(a,b) (c,d) -> d `compare` b) $ map (\a -> (a, fitnessFunction a)) population

getSubSelection :: [(Chromosome a,Float)] -> StdGen -> Int -> Population a
getSubSelection _ _ 0 = []
getSubSelection fpop gen n = let 
		(_, gen') = next gen
		indiv = rouletteSelection fpop gen
	in indiv:(getSubSelection fpop gen' (n-1))


rouletteSelection :: [(Chromosome a, Float)] -> StdGen -> Chromosome a
rouletteSelection xs g = R.evalRand (R.fromList $ map (\(a,b) -> (a, toRational b)) xs) g


evolve _ _ _ _ _ p _ 0 = p
evolve fitnessFunc crossRate mutateRate mutator crosser population popSize iterations = do
	let parentsN = truncate $ crossRate * (fromIntegral popSize)
	let copiesN = truncate $ (1-crossRate) * (fromIntegral popSize)
	let fpop = fittedPop fitnessFunc population
	gen <- newStdGen
	let copies = getSubSelection fittedPop gen copiesN
	cross <- crossoverPop crosser crossRate fpop
	newPop <- mutatePop mutator mutateRate (cross ++ copies)
	evolve fitnessFunc crossRate mutateRate mutator crossRate newPop popSize (iterations -1)

 
-- tests
type bins = Chromosome a

hamingS [] [] = 0
hammingS (t:ts) (x:xs) = (if t == x then 1 else 0) + hamming ts xs 

mutate a = 
	i <- randomRIO (0, length a - 1)
	b <- randomRIO (0,1)
	return $ a /// (i,b)

cross a b = do
	let n = length a
	i <- randomRIO (0,n-1)
	return $ (take i a) ++ (drop i b)

