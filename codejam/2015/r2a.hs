import qualified Data.Vector as V
import Control.Applicative
import System.Environment


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

isMapSafe mp = let
    n = V.length mp
    m = V.length $ mp V.! 0
    in map (\(i,j) -> isSolutionSafe mp (i,j) []) [(i,j) | i <- [0..m-1], j <- [0..n-1]]


outputAns i a = "Case #" ++ (show i) ++ ": " ++ (show a)

showAns [] _ = []
showAns (sentences:ts) i =
    (outputAns i $ getAns sentences) : showAns ts (i+1)

getAns t = 33



edges = [(0,1),(1,4),(4,3),(0,2),(2,4),(2,3),(1,2)]
buildAdjacency edges = let n = length edges
    in buildAdjacency' edges $ V.replicate n []
buildAdjacency' [] arr = arr
buildAdjacency' ((i,j):cs) arr =
    let cur = (arr V.! i)
        added = if j `elem` cur then cur else j  : cur
    in buildAdjacency' cs (arr V.// [(i,added)])

-- Returns [5,4,3,2,1,0], i.e. the path searched
-- testGraph =
--     [
--     [1,2,3], --0
--     [4],
--     [],
--     [],
--     [5],
--     []
--     ] :: [[Int]]
-- nodeWeights = [0,0,0,9,0,9] :: [Int]
-- testCond n = nodeWeights !! n == 9
--
-- bfs graph s cond = bfs' graph [s] [] []
--     where
--         bfs' graph [] path _ = path
--         bfs' graph (q:qs) path visited
--             | not (q `elem` visited) = bfs' graph (qs ++ (graph !! q)) (q:path) (q:visited)
--             | otherwise = bfs' graph qs path visited
--
--
testGraph =
    [
    [1,2,3], --0
    [4],
    [],
    [],
    [5],
    []
    ] :: [[Int]]
nodeWeights = [0,0,0,9,0,9] :: [Int]
testCond n = n==5

bfs graph s cond = bfs' graph [(s,[s])] []
    where
        bfs' graph [] _ = []
        bfs' graph ((v,p):qs) visited
            | cond v = reverse $ p
            | not (v `elem` visited) = bfs' graph
                        (qs ++ (map (\v' -> (v', v':p)) (graph V.! v) ))
                        (v:visited)
            | otherwise = bfs' graph qs visited






main = do
    fileName <- argOrInput 0
    contents <- readFile fileName
    let (h:c) = lines contents
    let testCases = linesToTestCase c []
    --let output = unlines $ showAns testCases 1
    --writeFile (fileName ++ ".out") output
    --putStrLn $ show $ testCases !! 0
    return $ testCases
