import Data.List
import Data.Array

type Edge = (Int, Int)

data Node = Node Integer [Node]


(///) (x:xs) (0,v) = v:xs
(///) (x:xs) (ind,v) = x : (///) xs (ind-1,v)  


createEmpty n = array (0,n-1) [(i,[]) | i <- [0..n-1]]

addNeighbor nodes (v1,v2) = add (add nodes v1 v2) v2 v1 where
	add nodes n1 n2 = let neighbors = nodes ! n1 in
		if not $ n2 `elem` neighbors then nodes // [(n1,n2:neighbors)] else nodes

isEdgeValid colors (v1,v2) = (colors !! v1) /= (colors !! v2)
isEdgeInvalid colors edge = not $ isEdgeValid colors edge

-- vs is the neighbors of v
numberOfNodeViolations v vs colors = let edges = map (\vi -> (v,vi)) vs in
	length $ filter (isEdgeInvalid colors) edges

-- maxi
bestMove v colors nodes = let 
						otherColors = map (\a -> colors /// (v,a)) [0..5]
						vs = nodes ! v
	in sortBy (\a b -> (numberOfNodeViolations v vs a) `compare` (numberOfNodeViolations v vs b))



lineToEdge :: String -> Edge
lineToEdge line = let [v1, v2] = (map read $ words line) :: [Int] in (v1,v2) 

main = do
	contents <- readFile "data/gc_4_1"
	let (h:l) = lines contents
	let [nodeCount, edgesCount] = (map read $ words h) :: [Int] 
	let edges = map lineToEdge l
	let nodeList = [0..nodeCount - 1]
	let nodes = foldl addNeighbor (createEmpty nodeCount) edges
	let colors = take nodeCount $ repeat 0
	putStrLn $ show nodes



--- ci in colors denotes the color of vi in nodes