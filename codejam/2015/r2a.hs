import qualified Data.Vector as V
import Control.Applicative
import System.Environment

argOrInput n = do
	args <- getArgs
	if length args < 1 then
		getLine
	else
		return $ args !! n

getNextArrowPos t (i,j) = let a = t V.! i V.! j in
    case a of
        '>'
        '^'
        'v'
        '<' ->
        '.' -> Just (i,j)
        otherwise -> Nothing

linesToTestCase [] testCases = reverse $ testCases
linesToTestCase (h:hs) t = let
        [i,j] = read <$> (words h)
        (rows, other) = splitAt i hs
        rowVecs = V.fromList $ V.fromList <$> rows
    in linesToTestCase other (rowVecs:t)

main = do
    fileName <- argOrInput 0
    contents <- readFile fileName
    let (h:c) = lines contents
    let testCases = linesToTestCase c []
    --let output = unlines $ showAns testCases 1
    --writeFile (fileName ++ ".out") output
    --putStrLn $ show $ testCases !! 0
    return $ testCases
