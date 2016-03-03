import Data.List
import System.Environment

argOrInput n = do
	args <- getArgs
	if length args < 1 then
		getLine
	else
		return $ args !! n

outputAns testCases solver = unlines $ outputAns' testCases 1 solver
outputAns' [] _ _ = []
outputAns' (t:ts) n solver = ("Case #" ++ (show n) ++ ": " ++ (show $ solver t)) : outputAns' ts (n+1) solver

arrToNum :: [Int] -> Int
arrToNum arr = read $ concat $ map show arr

insertAt [] x _ = [x]
insertAt xs x 0 = x : xs
insertAt (x:xs) xi n = x : (insertAt xs xi (n-1))
addNextZero xs = head $ sort $ map arrToNum $ map (insertAt xs' 0) [1..l]
    where
        l = length xs
        xs' = sort xs


genAns :: Int -> Int
genAns num = case length arr of
    0 -> addNextZero (sort numArrInt)
    otherwise -> head arr
    where
        numArr = show num
        numArrInt = map (\x -> read $ "" ++ [x]) numArr :: [Int]
        arr = sort $ filter (>num) $ map read $ permutations numArr

main = do
    fileName <- argOrInput 0
    contents <- readFile fileName
    let (h:c) = lines contents
    let numbers = map read c :: [Int]
    let output = outputAns numbers genAns
    writeFile (fileName ++ ".out") output
    putStrLn $ output
