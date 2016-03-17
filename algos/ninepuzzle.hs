import qualified Data.Vector as V
import Control.Monad
import Text.ParserCombinators.ReadPrec
import Text.Read
--  (Board, positionOfZero)
type BoardState = (Board, Pos)
type Board = V.Vector Int 
type Pos = (Int, Int)
instance (Num a, Num b) => Num (a,b) where
    (+) (q,w) (a,s) = (q+a,w+s)
startState = (V.fromList [1,8,2,0,4,3,7,6,5], (1,0)) :: BoardState
goalState = (V.fromList [1,2,3,4,5,6,7,8,0], (2,2)) :: BoardState


genSwaps a@(i,j) = filter (\(q,w) -> let d = (abs (q-i) + (abs (w-j)))
            in d == 1 && q >= 0 && q < 3 && w >= 0 && w < 3) $
                fmap (\b -> a+b) $ liftM2 (,) [-1,1,0] [1,-1,0]
swaps' = V.fromList $ map genSwaps [(i,j) | i <- [0..2], j<- [0..2]]
swap (i,j) = swaps' V.! (i*3 + j)

(!) :: Board -> Pos -> Int
board ! p = board V.! (o p)


splitter [] _ = []
splitter lis i = let (h,t) = splitAt i lis
    in h:splitter t i
visualize :: Board -> String
visualize board =   let e = map (\i -> i ++ "\t") $ map show $ V.toList board
                        xs = splitter e 3
                        s = map unwords xs
                    in unlines s

visualize' board = let (b,_) = board in putStr $ visualize b

-- o :: (Int, Int) -> Int
o (i,j) = (i*3 + j)
-- d :: (Int) -> (Int, Int)
d k = (k `div` 3, k `mod` 3) 


swapBoard :: Board -> Pos -> Pos -> Board
swapBoard state a b = let av = state ! a
                          bv = state ! b
                       in state V.// [(o a,bv),(o b,av)]

swapBoardState :: BoardState -> Pos -> BoardState
swapBoardState (board,zpos) pos = (swapBoard board zpos pos,pos)

playLoop :: BoardState -> IO String
playLoop boardState
  | boardState == goalState = return "You won"
playLoop bs@(board,zpos) = do
  visualize' bs
  putStr "\nEnter a move in the form of >>i j\n"
  ij <- getLine >>= return . (map readMaybe) . words 
  case ij of 
    [Just i, Just j] -> if (i,j) `elem` swap zpos then playLoop (swapBoardState bs (i,j)) else playLoop bs
    _ -> playLoop bs


branch :: BoardState -> [BoardState]
branch bs = zip (map (\n -> swapBoard board zpos n) neighbors) neighbors
    where neighbors = swap zpos 
          (board,zpos) = bs

solver states visited moves
  | goalState `elem` states = goalState : (reverse moves)
solver states visited moves = solver nextMoves (states ++ visited) 
  where
    nextMoves = filter (not . flip elem visite map branch states


main = do
        playLoop startState
