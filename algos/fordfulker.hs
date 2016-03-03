-- http://www.geeksforgeeks.org/ford-fulkerson-algorithm-for-maximum-flow-problem/
-- https://en.wikipedia.org/wiki/Ford%E2%80%93Fulkerson_algorithm

import Data.List as L
import Data.Array
import Control.Monad

caps :: [((Int, Int), Double)]
caps = [
    ((0,1), 16),
    ((1,3), 12),
    ((3,5), 20),
    ((0,2), 13),
    ((1,2), 10),
    ((2,1), 4),
    ((3,2), 9),
    ((2,4), 14),
    ((4,3), 7),
    ((4,5), 4)]

capAt = buildCapArray 5 caps

getDirection e cap = let c = cap ! e
    in if c >= 0 then 1 else -1

flow :: Array (Int, Int) Double
flow = array ((0,0), (5,5)) [(i,5) | i <- liftM2 (,) [0..5] [0..5]]

adjacencyt = buildAdjacency caps 5

buildCapArray b cap = array ((0,0), (b,b)) $ cap ++ ((map (\((i,j),v) -> ((j,i),-v)) cap))

buildAdjacencyBiDir cap b = buildAdjacency' (map (\((i,j),v) -> ((j,i),v)) cap) $ buildAdjacency cap b
buildAdjacency cap b = buildAdjacency' cap (array (0,b) [(i,[]) | i <- [0..b]])
buildAdjacency' [] arr = arr
buildAdjacency' (((i,j),v):cs) arr =
    let cur = (arr ! i)
        added = if j `elem` cur then cur else j  : cur
    in buildAdjacency' cs (arr // [(i,added)])

lookupDef val list def = maybe def id (lookup val list)

isFlowSatisfied e@(i,j) flowA capA =
    let f = flowA ! e
        c = capA ! e
    in (f >= 0) && (f <= c)

-- returns [(1,3), (3,4), (4,7)] etc
bfsPath adj s e cond = let path = bfs adj s e [] [] cond
    in tail $ zip (0:path) path
bfs _ s e path _ condition
    | s == e = Just $ reverse (e:path)
bfs adjacency s e path visited condition
    | neighbors == [] = Nothing
    | otherwise = head $ map
            (\a -> bfs adjacency a e (s:path) (s:visited) condition) neighbors
    where neighbors = filter (\a -> condition (s,a)) $ filter (not . flip elem visited) $ adjacency ! s
