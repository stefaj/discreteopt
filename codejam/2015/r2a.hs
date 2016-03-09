import Data.List
import System.Environment
import Data.Text (pack, unpack, splitOn)
import Control.Monad
import Control.Applicative
import qualified Data.Vector as V

argOrInput n = do
	args <- getArgs
	if length args < 1 then
		getLine
	else
		return $ args !! n

type Map = V.Vector (V.Vector Char)

linesToTestCase :: [String] -> [Map] -> [Map]
linesToTestCase [] testCases = reverse $ testCases
linesToTestCase (h:hs) t = let
        [i,j] = map read $ words h
        (rows, other) = splitAt i hs
        rows' = V.fromList $ map V.fromList rows
    in linesToTestCase other (rows':t)

nextArrow :: Map -> (Int, Int) -> Maybe (Int, Int)
nextArrow mp (i,j)
    | cur == '>' = scanRight $ i+1
    | cur == '<' = scanLeft $ i-1
    | cur == 'v' = scanDown $ j+1
    | cur == '^' = scanUp $ j-1
    | otherwise = Just (i,j)
    where
            cur = mp V.! j V.! i
            n = V.length mp
            m = V.length (mp V.! 0)
            scanRight i'
                | i' >= m = Nothing
            scanRight i' = if mp V.! j V.! i' /= '.' then Just (i',j) else scanRight (i'+1)
            scanLeft i'
                | i' < 0 = Nothing
            scanLeft i' = if mp V.! j V.! i' /= '.' then Just (i',j) else scanLeft (i'-1)
            scanDown j'
                | j' >= n = Nothing
            scanDown j' = if mp V.! j' V.! i /= '.' then Just (i,j') else scanDown (j'+1)
            scanUp j'
                | j' < 0 = Nothing
            scanUp j' = if mp V.! j' V.! i /= '.' then Just (i,j') else scanUp (j'+1)


isSolutionSafe mp (i,j) visited
        | (i,j) `elem` visited = True
isSolutionSafe mp (i,j) visited =
     case nextArrow mp (i,j) of
                Nothing -> False
                Just (i',j') -> isSolutionSafe mp (i',j') ((i,j):visited)






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
    return $ testCases
