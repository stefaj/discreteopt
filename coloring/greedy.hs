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


areEdgesValid v nodes colors = and $ map (isEdgeValid colors) edges
	where
		edges = map (\a -> (v,a)) (nodes ! v)

numberOfNodeViolations' edges colors = map (\(v1,v2) -> if isEdgeValid colors (v1,v2) then 0 else 1) edges

-- idea, give highest degree nodes distinct colors
-- start with worst solution of a lot of colors and optimize

-- returns colors

initializeColors nodes = indices nodes
-- initializeColors (c:colors) nodes = 
--	where 
--		aux [] _ = []
--		aux (v:verts) (c:color) = 
--		emptyColorArr = array (0,snd $ bounds nodes)
--		orderedVertices = reverse sortBy (\a b ->  (nodeDegree a nodes) `compare` (nodeDegree b nodes)) (indices nodes)  


colorCount colors = (maximum colors)+1

reduceColor v nodes colors = head $ sortBy (\a b -> (colorCount a) `compare` (colorCount b)) betterColors
	where
		n = length colors
		betterColors = filter (areEdgesValid v nodes) $ map (\a-> colors /// (v,a)) [0..n-1]


iterate' nodes colors 0 = colors
iterate' nodes colors n = iterate' nodes (changeColor orderedVertices colors) (n-1)
	where
		changeColor [] colors = colors
		changeColor (v:vs) colors' = changeColor vs (reduceColor v nodes colors')
		orderedVertices = reverse $ sortBy (\a b ->  (nodeDegree a nodes) `compare` (nodeDegree b nodes)) (indices nodes)  


nodeDegree v nodes = length $ nodes ! v




lineToEdge :: String -> Edge
lineToEdge line = let [v1, v2] = (map read $ words line) :: [Int] in (v1,v2) 


argOrInput n = do
	args <- getArgs
	if length args < 1 then
		getLine
	else
		return $ args !! n

main = do
	
	--contents <- readFile "data/gc_20_1"
	contents <- argOrInput 0 >>= readFile
	itStr <- argOrInput 1
	let iterations = read itStr :: Int
	
	-- let iterations = 500
	let (h:l) = lines contents
	let [nodeCount, edgesCount] = (map read $ words h) :: [Int] 
	let edges = map lineToEdge l
	let nodeList = [0..nodeCount - 1]
	let nodes = foldl addNeighbor (createEmpty nodeCount) edges
	let colors = initializeColors nodes
	
	-- let noColors = (maximum ans) + 1
	-- putStrLn $ (show noColors) ++ " 0"
	-- putStrLn $ unwords $ map show ans

	
	--putStrLn $ show colors
	--putStrLn $ show $ sum $ numberOfNodeViolations' edges colors
	--putStrLn $ show $ colorCount colors

	

	let ans = iterate' nodes colors iterations
	--putStrLn $ show ans
	putStrLn $ (show $ colorCount ans) ++ " 0"
	putStrLn $ unwords $ map show ans
	--putStrLn $ show $ sum $ numberOfNodeViolations' edges ans
	


--- ci in colors denotes the color of vi in nodes