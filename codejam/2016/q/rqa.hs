import Data.List
import System.Environment
import Data.Text (pack, unpack, splitOn)
import Control.Monad
import Control.Applicative

argOrInput n = do
	args <- getArgs
	if length args < 1 then
		getLine
	else
		return $ args !! n

linesToTestCase [] testCases = reverse $ testCases
linesToTestCase (h:hs) t = let
        n = read h
        (sentences, other) = splitAt n hs
    in linesToTestCase other (sentences:t)


outputAns i a = "Case #" ++ (show i) ++ ": " ++ (show a)

showAns [] _ = []
showAns (sentences:ts) i =
    (outputAns i $ getAns sentences) : showAns ts (i+1)

getAns t = 33


main = do
    fileName <- argOrInput 0
    contents <- readFile fileName
    let (h:c) = lines contents
    let testCases = linesToTestCase c []
    --let output = unlines $ showAns testCases 1
    --writeFile (fileName ++ ".out") output
    --putStrLn $ show $ testCases !! 0
    putStrLn $ show testCases
