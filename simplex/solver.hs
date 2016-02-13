import Data.Eigen.Matrix hiding (map, filter)
import Foreign.C.Types (CDouble)
import qualified Data.List as L
import Data.Maybe (fromJust)
import Prelude hiding (all)





b :: Matrix Double CDouble
b = fromList [[180],[300],[240]]

a :: Matrix Double CDouble
a = fromList [[2,1,1,1,0,0],
	[1,3,2,0,1,0],
	[2,1,2,0,0,1]]

c :: Matrix Double CDouble	
c = fromList [[6],[5],[4],[0],[0],[0]]

(///) (x:xs) (0,v) = v:xs
(///) (x:xs) (ind,v) = x : (///) xs (ind-1,v)  		

--xb =

fromCols cols m = transpose $ fromList $ map (flip col m) cols
fromRows rows m = transpose $ fromList $ map (flip row m) rows



rowMult m1 m2 = let
	m1r = col 0 m1 :: [Double]
	m2r = col 0 m2 :: [Double]
	mult [] _ = []
	mult (x:xs) (y:ys) = x*y:mult xs ys
	in transpose $ fromList $ [mult m1r m2r]

rowDiv m1 m2 = let
	m1r = col 0 m1 :: [Double]
	m2r = col 0 m2 :: [Double]
	mult [] _ = []
	mult (x:xs) (y:ys) = x/y:mult xs ys
	in transpose $ fromList $ [mult m1r m2r]	




-- simplex' a b c xn xb 0 = (bi, ni, xn,xb,red,rod,enter+1,exit+1)
-- 	where
-- 		bi = fromCols xb a
-- 		ni = fromCols xn a
-- 		cb = fromRows xb c
-- 		cn = fromRows xn c
-- 		red' = cn - cb * (inverse bi) * ni 
-- 		red = head $ toList red' -- if red < 0 then stop
-- 		enteri = fromJust $ L.elemIndex (maximum red) red
-- 		enter = xn !! enteri
-- 		aj = fromCols [enter] a
-- 		rod = head $ toList $ transpose $ rowDiv (inverse bi * b) (inverse bi * aj)
-- 		exiti = fromJust $ L.elemIndex (minimumS rod) rod
-- 		exit = xb !! exiti
-- 		xn' = xn /// (enteri,exit)
-- 		xb' = xb /// (exiti, enter) 



simplex a b c = let
		
		in simplex' a b c xn xb
	where
		nvars = 3
		--p = cols b -- conditions
		n = cols a -- vars
		xn = [0..2]
		xb = [3..5] 
		simplex' a b c xn xb = 
				if all (< 0) red'
				then 
					--(xb,xn)
					(take nvars $ head $ toList $ transpose $ inverse (fromCols (L.sort xb) a) * b, L.sort xb)
				else 								
					simplex' a b c xn' xb'
			where
				bi = fromCols xb a
				ni = fromCols xn a
				cb = fromRows xb c
				cn = fromRows xn c
				red = head $ toList red' -- if red < 0 then stop
				enteri = fromJust $ L.elemIndex (maximum red) red
				enter = xn !! enteri
				aj = fromCols [enter] a
				rod = head $ toList $ transpose $ rowDiv (inverse bi * b) (inverse bi * aj)
				exiti = fromJust $ L.elemIndex (minimumS rod) rod
				exit = xb !! exiti
				xn' = xn /// (enteri,exit)
				xb' = xb /// (exiti, enter) 				
				red' = cn - cb * (inverse bi) * ni 
				bi' = fromCols xb' a


minimumS' [] v = v
minimumS' (x:xs) v = if x < v && x >= 0 then minimumS' xs x else minimumS' xs v
minimumS xs = minimumS' xs (head $ filter (>=0) xs)


