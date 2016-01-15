import Data.List
import Data.Array
import System.Environment

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

numberOfNodeViolations' edges colors = map (\(v1,v2) -> if isEdgeValid colors (v1,v2) then 0 else 1) edges

-- maxi
bestMove v colors nodes = let 
						otherColors = map (\a -> colors /// (v,a)) [0..3]
						vs = nodes ! v
						edges = map (\vi -> (v,vi)) vs
	in head $ sortBy (\a b -> (numberOfNodeViolations' edges a) `compare` (numberOfNodeViolations' edges b)) otherColors


iterate' nodes colors 0 = colors
iterate' nodes colors n = iterate' nodes (bestMove worstNode colors nodes) (n-1)
		where 	
			zipEdges v = map (\a -> (v,a)) $ nodes ! v
			worstNode = head $ reverse $ sortBy (\a b -> (numberOfNodeViolations' (zipEdges a) colors) `compare` (numberOfNodeViolations' (zipEdges b) colors)) (indices nodes)
				
		

lineToEdge :: String -> Edge
lineToEdge line = let [v1, v2] = (map read $ words line) :: [Int] in (v1,v2) 


argOrInput n = do
	args <- getArgs
	if length args < 1 then
		getLine
	else
		return $ args !! n

main = do
	
	--contents <- readFile "data/gc_70_1"
	contents <- argOrInput 0 >>= readFile
	--itStr <- argOrInput 1
	--let iterations = read itStr :: Int
	let iterations = 500
	let (h:l) = lines contents
	let [nodeCount, edgesCount] = (map read $ words h) :: [Int] 
	let edges = map lineToEdge l
	let nodeList = [0..nodeCount - 1]
	let nodes = foldl addNeighbor (createEmpty nodeCount) edges
	let colors = take nodeCount $ concat $ repeat [0..7]
	let ans = iterate' nodes colors iterations
	let noColors = (maximum ans) + 1
	putStrLn $ (show noColors) ++ " 0"
	putStrLn $ unwords $ map show ans

	--putStrLn $ show $ sum $ numberOfNodeViolations' edges ans




--- ci in colors denotes the color of vi in nodes