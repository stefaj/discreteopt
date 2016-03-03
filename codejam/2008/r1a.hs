import Data.List
import System.Environment

argOrInput n = do
	args <- getArgs
	if length args < 1 then
		getLine
	else
		return $ args !! n


linesToTestCase [] = []
linesToTestCase (n:l1:l2:xs) = (l1',l2') : linesToTestCase xs
  where
    l1' = (map (read) $ words l1) :: [Int]
    l2' = (map (read) $ words l2) :: [Int]


dot [] [] = 0
dot (x:xs) (y:ys) = x*y + dot xs ys

bestAnsTestCase (v1,v2) = dot (sort v1) (reverse $ sort v2)

outputAns [] _ = []
outputAns (t:ts) n = ("Case #" ++ (show n) ++ ": " ++ (show $ bestAnsTestCase t)) : outputAns ts (n+1)



main = do
  fileName <- argOrInput 0
  contents <- readFile fileName
  let (h:c) = lines contents
  let testCases = linesToTestCase c
  let output = unlines $ outputAns testCases 1
  writeFile (fileName ++ ".out") output
  putStr output
