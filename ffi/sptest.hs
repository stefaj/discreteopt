{-# OPTIONS_GHC -Wall #-}

module Main where

import qualified Data.Vector as V
import qualified Data.Array as A

import Foreign.C

import CPLEX.Param
import CPLEX


graph = [
            [1,2],  -- 0
            [3,4],  -- 1
            [0,3],    -- 2
            [],     -- 3
            []      -- 4
        ] :: [[Int]]

edgeWeights :: A.Array (Int, Int) Int
edgeWeights = A.array ((0,0),(4,4)) $  [((0,1),3),((1,4),1),((0,2),2),((2,0),2),((2,3),1),((1,3),1)]

xij = [(i,j) | i <- [0..4], j <- [0..4]]


go i = [au i' j | i' <- [0..4], j <- [0..4]]
    where
        au i' j
            | not $ j `elem` (graph !! i') = 0
            | i==i' && ((i',j) `elem` xij) = 1
            | i==j && ((j,i') `elem` xij) = -1
            | otherwise = 0

getRhs = map getRhs' [0..4]
getRhs' i
    | i == s = E 1
    | i == t = E (-1)
    | otherwise = E 0

getObj = map aux xij
    where
        aux (i,j)
            | j `elem` (graph !! i) = fromIntegral $ edgeWeights A.! (i,j)
            | otherwise = 20


-- solver s t = let
--     obj = V.fromList $ A.elems xij
--     aux i
--         | i == s = 1
--         | i == t = -1
--         | otherwise = 0
--     map $ (\i -> sum )
--






cpx_ON :: CInt
cpx_ON  =  1
cpx_OFF :: Integer
cpx_OFF =  0

main :: IO ()
main = do
    let obj = V.fromList getObj
    let rhs = V.fromList $ getRhs
    let xbnds = take (length xij) $ repeat (Just 0, Nothing)
    let st = map go [0..4]
    let sparsed = map (\(x,y) -> (Row y, Col x, st !! y !! x )) [(x,y) | x <- [0..length xij - 1], y <- [0..4]]
    putStrLn $ show $ obj
    putStrLn $ show $ rhs
    --putStrLn $ show $ xbnds
    --putStrLn $ show $ st
    putStrLn $ show $ sparsed
    sol'

--main = sol' >>= print


s = 0
t = 3

sol' :: IO ()
sol' = withEnv $ \env -> do
  setIntParam env CPX_PARAM_SCRIND cpx_ON

  setIntParam env CPX_PARAM_DATACHECK cpx_ON
  withLp env "testprob" $ \lp -> do
    let objsen = CPX_MAX
        obj = V.fromList getObj
        rhs = V.fromList $ getRhs
        xbnds = take (length xij) $ repeat (Just 0, Nothing)
        st = map go [0..4]
        sparsed = map (\(x,y) -> (Row y, Col x, st !! y !! x )) [(x,y) | x <- [0..2], y <- [0..1]]

    statusLp <- copyLp env lp objsen obj rhs sparsed (V.fromList xbnds)

    case statusLp of
      Nothing -> return ()
      Just msg -> error $ "CPXcopylp error: " ++ msg

    statusOpt <- qpopt env lp
    case statusOpt of
      Nothing -> return ()
      Just msg -> error $ "CPXqpopt error: " ++ msg

    statusSol <- getSolution env lp
    case statusSol of
      Left msg -> error $ "CPXsolution error: " ++ msg
      Right sol -> do
        putStrLn $ "x      : " ++ show (solX sol)
        putStrLn $ "pi'    : " ++ show (solPi sol)
        putStrLn $ "slack  : " ++ show (solSlack sol)
        putStrLn $ "dj     : " ++ show (solDj sol)
        putStrLn $ "solstat: " ++ show (solStat sol)
        putStrLn $ "objval : " ++ show (solObj sol)
