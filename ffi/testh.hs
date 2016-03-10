{-# OPTIONS_GHC -Wall #-}

module Main where

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

import Foreign.C

import CPLEX.Param
import CPLEX
import CPLEX.Bindings
import System.IO.Unsafe (unsafePerformIO)
import Foreign.ForeignPtr.Safe(newForeignPtr_)


cb :: CpxLp -> CIncumbentCallback
cb lp env _ wherefrom _ objval xptr feasptr usrptr = do
    foreignPtr <- newForeignPtr_ xptr
    let xs = VS.unsafeFromForeignPtr0 foreignPtr 3
    let _ = unsafePerformIO $ putStrLn "called"
    --addCuts (CpxEnv env) lp 1 (V.fromList [L 10]) [(Row 0,Col 0, 0)]
    return 2

cpx_ON :: CInt
cpx_ON  =  1
cpx_OFF :: Integer
cpx_OFF =  0

main :: IO ()
main = sol' >>= print

sol' :: IO ()
sol' = withEnv $ \env -> do
    -- prints some useful output stuff
  --setIntParam env CPX_PARAM_SCRIND cpx_ON

  -- CHECKS if data is shap
 -- setIntParam env CPX_PARAM_DATACHECK cpx_ON
  withLp env "testprob" $ \lp -> do
    let objsen = CPX_MAX
        obj = V.fromList [1,2,3]
        rhs = V.fromList [L 20, L 30]
        xbnds = [(Just 0, Just 40), (Just 0, Nothing), (Just 0, Nothing)]

        st = [[-1,1,1], [1,-3,1]]
        sparsed = map (\(x,y) -> (Row y, Col x, st !! y !! x )) [(x,y) | x <- [0..2], y <- [0..1]]
    putStrLn $ show obj
    putStrLn $ show sparsed
    putStrLn $ show xbnds
    statusLp <- copyLp env lp objsen obj rhs sparsed (V.fromList xbnds)
    case statusLp of
      Nothing -> return ()
      Just msg -> error $ "CPXcopylp error: " ++ msg

    statcuts <- addRows env lp 0 3 3 (V.fromList [G 20]) [(Row 2, Col 1, 1 )]
    case statcuts of
        Nothing -> return ()
        Just msg -> error $ "CPXcopylp error: " ++ msg

    --cbstat <- setIncumbentCallback env (cb lp)
    --case cbstat of
    --    Nothing -> putStrLn "callback registered"
    --    Just msg -> putStrLn $ "callback error: " ++ msg

    ------------------------
    -- let qmat = [ (Row 0, Col 0, -33)
    --            , (Row 1, Col 0, 6)
    --            , (Row 0, Col 1, 6)
    --            , (Row 1, Col 1, -22)
    --            , (Row 2, Col 1, 11.5)
    --            , (Row 1, Col 2, 11.5)
    --            , (Row 2, Col 2, -11)
    --            ]
    -- statusQuad <- copyQuad env lp qmat
    -- case statusQuad of
    --   Nothing -> return ()
    --   Just msg -> error $ "CPXcopyquad error: " ++ msg

    ------------------------


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
