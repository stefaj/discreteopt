import Data.List
import Data.Array

-- root node and connected edges
data Node a = Node a [Node a] | Empty
	deriving (Show, Eq)


-- this will only work if we assume the graph is simple. 
traverseDF Empty = []
traverseDF (Node a ns) = a:(concat $ map traverseDF ns)

traverseBF' Empty = []
traverseBF' n@(Node b xs) = (helper n) ++ (concat $ map traverseBF' xs)
	where
		helper (Node a ns) = map (\(Node a _) -> a ) ns
traverseBF n@(Node b xs) = b:traverseBF' n

testNode = Node 0 [
			Node 1 [Node 2 []], 
			Node 3 [], 
			Node 5 [], 
			Node 7 [
				Node 4 [], Node 6 [], Node 8 [], Node 10 [Node 11 []]],
			Node 9 []
					]




-- Adjacency List
type GraphA = Array Int Int

tG :: GraphA
tG = array (0,10) [(i,v) | i <- [0..10], let v = 3]

dfs graph visited [] = reverse visited
dfs graph visited (x:xs)
	| x `elem` visited = dfs graph visited xs
	| otherwise = dfs graph (x:visited) ((graph ! x) ++ xs)