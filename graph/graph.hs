import Data.Array

(///) (x:xs) (0,v) = v:xs
(///) (x:xs) (ind,v) = x : (///) xs (ind-1,v)


removeFromList v [] = []
removeFromList v (x:xs) = if v == x then xs else x:(removeFromList v xs)

graph = [
            [1,2],  -- 0
            [3,4],  -- 1
            [0,3],    -- 2
            [],     -- 3
            []      -- 4
        ] :: [[Int]]

edgeWeights :: Array (Int, Int) Int
edgeWeights = array ((0,0),(4,4)) $  [((0,1),3),((1,4),1),((0,2),2),((2,0),2),((2,3),1),((1,3),1)]


dijkstra s t graph weights = let n = length graph in
    dijkstra' s t [] [0..n] $ array (0,n) [(x,v) | x <- [0..n], let v = (if x==s then 0 else 99999)]
    where
        dijkstra' s t _ _ tentatives
            | s == t = tentatives ! t
        dijkstra' s t _ [] _ = -1
        dijkstra' s t visited (x:xs) tentatives =
            dijkstra' x t (s:visited) xs (tentatives // neighborDists)
            where
                dist = tentatives ! s
                neighborDists = map (\k -> (k, minimum [dist + (weights ! (s,k)), tentatives ! k])
                                    ) [x | x <- graph !! s, not $ x `elem` visited]
