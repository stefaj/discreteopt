{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

import Foreign.C

import CPLEX.Param
import CPLEX
import CPLEX.Bindings
import System.IO.Unsafe(unsafePerformIO)
import Foreign.ForeignPtr(newForeignPtr_)
import Foreign.Ptr(nullPtr, nullFunPtr, freeHaskellFunPtr)
import Foreign.Storable(peek, poke)


{-# NOINLINE cb #-}
cb :: CCutCallback
cb env' cbdata wherefrom cbhandle ptrUser = do
    let env = CpxEnv env'
    -- foreignPtr <- newForeignPtr_ xptr
    -- let xs = VS.unsafeFromForeignPtr0 foreignPtr 3
    (_, xs) <- getCallbackNodeX env cbdata (fromIntegral wherefrom) 0 2
    putStrLn $ "pudding pie xs " ++ (show xs)
    lpstat <- getCallbackLP env cbdata (fromIntegral wherefrom)

    case lpstat of
        Left err -> putStrLn $ "Cobus Callback Error " ++ err
        Right lp -> do

            if xs VS.! 2 > 20 then do
                putStrLn "x too high, cutting"
                addCutFromCallback env cbdata wherefrom 3 (L 20) [(Col 0,0), (Col 1,0), (Col 2, 1)] CPX_USECUT_FORCE
                putStrLn "cut added"
                --poke feasptr 0
                -- statcuts <- addLazyConstraints env lp 3 (V.fromList [L 20]) [(Row 0, Col 0, 0 ), (Row 0, Col 1, 0 ), (Row 0, Col 2, 1 )]
                -- case statcuts of
                --     Nothing -> return ()
                --     Just msg -> error $ "CPXcopylp error: " ++ msg
            else
                return ()
    return 0

    --foreignPtr <- newForeignPtr_ xptr
    --let xs = VS.unsafeFromForeignPtr0 foreignPtr 3
    --let _ = unsafePerformIO $ putStrLn "called"
    --addCuts (CpxEnv env) lp 1 (V.fromList [L 10]) [(Row 0,Col 0, 0)]


cpx_ON :: CInt
cpx_ON  =  1
cpx_OFF :: CInt
cpx_OFF =  0

main :: IO ()
main = sol' >>= print

gol' :: IO ()
sol' = withEnv $ \env -> do
    -- prints some useful output stuff
  --setIntParam env CPX_PARAM_SCRIND cpx_ON

  -- CHECKS if data is shap
 -- setIntParam env CPX_PARAM_DATACHECK cpx_ON
  setIntParam env CPX_PARAM_MIPCBREDLP cpx_OFF
  setIntParam env CPX_PARAM_PRELINEAR cpx_OFF
  withLp env "testprob" $ \lp -> do
    let objsen = CPX_MAX
        obj = V.fromList [1,2,3]
        rhs = V.fromList [L 20, L 30]
        xbnds = [(Just 0, Just 40), (Just 0, Nothing), (Just 0, Nothing)]

        st = [[-1,1,1], [1,-3,1]]
        sparsed = map (\(x,y) -> (Row y, Col x, st !! y !! x )) [(x,y) | x <- [0..2], y <- [0..1]]
        types = [CPX_INTEGER,CPX_INTEGER,CPX_CONTINUOUS]
    putStrLn $ show obj
    putStrLn $ show sparsed
    putStrLn $ show xbnds
    statusLp <- copyMip env lp objsen obj rhs sparsed (V.fromList xbnds) (V.fromList types)
    case statusLp of
      Nothing -> return ()
      Just msg -> error $ "CPXcopylp error: " ++ msg

    -- statcuts <- addLazyConstraints env lp 3 (V.fromList [L 20]) [(Row 0, Col 0, 0 ), (Row 0, Col 1, 0 ), (Row 0, Col 2, 1 )]
    -- case statcuts of
    --     Nothing -> return ()
    --     Just msg -> error $ "CPXcopylp error: " ++ msg
    let CpxEnv env' = env
    --ptr <- c_createCutCallbackPtr cb
    status <- setCutCallback env cb

    case status of
      Nothing -> return ()
      Just msg -> putStrLn $ "ERROR ON THE CALLBACK HOOK: " ++ msg
    putStrLn "cb registered"

    statusOpt <- mipopt env lp
    case statusOpt of
      Nothing -> return ()
      Just msg -> error $ "CPXqpopt error: " ++ msg

    putStrLn "solved mip"


    statusSol <- getMIPSolution env lp
    case statusSol of
      Left msg -> error $ "CPXsolution error: " ++ msg
      Right sol -> do
        putStrLn $ "x      : " ++ show (solX sol)
        putStrLn $ "pi'    : " ++ show (solPi sol)
        putStrLn $ "slack  : " ++ show (solSlack sol)
        putStrLn $ "dj     : " ++ show (solDj sol)
        putStrLn $ "solstat: " ++ show (solStat sol)
        putStrLn $ "objval : " ++ show (solObj sol)
