import Data.List
import Data.Array
import Data.Char (ord)

-- root node and connected edges
data Node a = Node a [Node a] | Empty
	deriving (Show, Eq)


nodeVal (Node a _ ) = a

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


toAdjacency n@(Node a ns) array = foldr toAdjacency (helper n array) ns
	where
		helper (Node a []) array = array
		helper (Node a (n:ns)) array = let 
			edges = array ! a
			add v l = if v `elem` l then l else v:l
			nv = nodeVal n in
		 helper (Node a ns) (array // [(a, add nv edges), (nv, add a (array ! nv))])




-- Structuring DFS

type Vertex = Int
type Table a = Array Vertex a
type Graph = Table [Vertex]

vertices :: Graph -> [Vertex]
vertices = indices



type Edge = (Vertex, Vertex)

type Bounds = (Vertex, Vertex)

edges :: Graph -> [Edge]
edges g = [(v,w) | v <- vertices g, w <- g ! v]


mapT :: (Vertex -> a -> b) -> Table a -> Table b
mapT f t = array (bounds t) [(v, f v (t!v))  | v <- indices t]

outdegree :: Graph -> Table Int
outdegree g = mapT numEdges g where numEdges v ws = length ws



buildG :: Bounds -> [Edge] -> Graph
buildG bnds es = accumArray (flip (:)) [] bnds es

graph = buildG (ord 'a',ord  'j')
	[(ord 'a',ord 'j'),(ord 'a',ord 'g'),(ord 'b',ord 'i'),
	(ord 'b',ord 'a'),(ord 'c',ord 'h'),(ord 'c',ord 'e'),
	(ord 'e',ord 'j'),(ord 'e',ord 'h'),(ord 'e',ord 'd'),
	(ord 'f', ord 'i'),(ord 'g',ord 'f'),(ord 'g',ord 'b')]

transposeG :: Graph -> Graph
transposeG g = buildG (bounds g) (reverseE g)

reverseE :: Graph -> [Edge]
reverseE g = [(w,v) | (v,w) <- edges g]

indegree :: Graph -> Table Int
indegree g = outdegree $ transposeG g



data Tree a = NodeT a (Forest a)
type Forest a = [Tree a]