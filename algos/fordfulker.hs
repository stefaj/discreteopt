-- http://www.geeksforgeeks.org/ford-fulkerson-algorithm-for-maximum-flow-problem/
-- https://en.wikipedia.org/wiki/Ford%E2%80%93Fulkerson_algorithm

import Data.List as L
import Data.Array
import Data.Maybe
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

getDirection e capA = let c = capA ! e
    in if c >= 0 then 1 else -1

flow :: Array (Int, Int) Double
flow = array ((0,0), (5,5)) [(i,0) | i <- liftM2 (,) [0..5] [0..5]]

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
        forward = getDirection e capA > 0
    in (f > 0 && (not forward)) || (f < c && forward)

-- this is actually dfs

-- returns [(1,3), (3,4), (4,7)] etc
bfsPath adj s e cond = do
    path <- bfs adj s e [] [] cond
    return $ tail $ zip (0:path) path
bfs _ s e path _ condition
    | s == e = Just $ reverse (e:path)
bfs adjacency s e path visited condition
    | neighbors == [] = Nothing
    | otherwise = head $ (filter (\a -> isJust a) $ map
            (\a -> bfs adjacency a e (s:path) (s:visited) condition) neighbors) ++ [Nothing]
    where neighbors = filter (\a -> condition (s,a)) $ filter (not . flip elem visited) $ adjacency ! s

fordFulkers :: Int -> Int -> [((Int, Int), Double)] -> Int -> Array (Int, Int) Double
fordFulkers s t cap b =
    let flow = array ((0,0), (b,b)) [(i,0) | i <- liftM2 (,) [0..b] [0..b]]
    in search flow
    where
        biAdj = buildAdjacencyBiDir cap b
        adj = buildAdjacency cap b
        capA = buildCapArray b cap
        search :: Array (Int, Int) Double -> Array (Int, Int) Double
        search flow = let
            cond e = isFlowSatisfied e flow capA
            path = bfsPath biAdj s t cond
            in case path of
                (Just path') -> search $ augmentPath flow (getMaxRes flow capA path') path' capA
                Nothing -> flow

getMaxRes flow capA path = getMaxRes' flow capA path 99999999999
getMaxRes' flow capA [] m = m
getMaxRes' flow capA (e:paths) m = let
    c = capA ! e
    f = flow ! e
    in getMaxRes' flow capA paths $ minimum [m, if (getDirection e capA) > 0 then c - f else f - 0]

augmentPath flow _ [] _ = flow
augmentPath flow val (e:paths) capA = let
    curfe = flow ! e
    forward = (getDirection e capA) > 0
    newfe = if forward then curfe + val else curfe - val
    in augmentPath (flow // [(e,newfe)]) val paths capA

getFlowTo p flow b = sum $ map (\a -> flow ! (a,p)) [0..5]
getFlowFrom p flow b = sum $ map (\a -> flow ! (p,a)) [0..5]
divergence p flow b = (getFlowFrom p flow b) - (getFlowTo p flow b)
