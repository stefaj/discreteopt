import Data.Array
import Control.Monad
import Data.Maybe (isJust, Maybe, fromJust)

instance (Num a, Num b) => Num (a,b) where
    (+) (q,w) (a,s) = (q+a,w+s)

genState seque = array ((0,0),(3,3)) $ let i = liftM2 (,) [0..3] [0..3]
                                in zip i seque

testArr = genState [9,2,8,11,0,5,13,7,15,1,4,10,3,14,6,12]
endState = genState [9,2,8,11,5,1,13,7,15,0,4,10,3,14,6,12]

genNeighbors state = let
        (zeroPos, _) = head $ filter (\(i,e) -> e==0) $ assocs state
    in map (\s -> swap state s zeroPos) $ genSwaps zeroPos
    where
        genSwaps a@(i,j) = filter (\(q,w) -> let d = (abs (q-i) + (abs (w-j)))
            in d == 1 && q >= 0 && q < 4 && w >= 0 && w < 4) $
                map (\b -> a+b) $ liftM2 (,) [-1,1,0] [1,-1,0]
        swap state a b = let    av = state ! a
                                bv = state ! b
                        in state // [(a,bv),(b,av)]

-- && q >= 0 && q < 4 && w >= 0 && w < 4
search state end _  moves
    | state == end = Just moves

search state end visited moves
    | neighbors == [] = Nothing
    | otherwise = head $ (filter (\a -> isJust a) $ map
            (\a -> search a end (state:visited) (state:moves)) neighbors) ++ [Nothing]
    where neighbors = filter (not . flip elem visited) $ genNeighbors state



visualize state =   let e = map (\i -> i ++ "\t") $ map show $ elems state
                        xs = splitter e 4
                        s = map unwords xs
                    in unlines s

visualize' state = putStr $ visualize state

splitter [] _ = []
splitter lis i = let (h,t) = splitAt i lis
    in h:splitter t i

main = do
        let s = fromJust $ search testArr endState [] []
        putStrLn $ unlines $ map visualize s
