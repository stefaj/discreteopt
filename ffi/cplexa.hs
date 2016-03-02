{-# OPTIONS_GHC -Wall #-}


import Data.Ix as I
import qualified Data.Vector as V
import qualified Data.Sequence as S
import Data.Foldable as F
import CPLEX.Param
import CPLEX hiding (Bound)
import Foreign.C (CInt)

data Variable a =  Double :# a

instance (Show a) => Show (Variable a) where
    show (d :# v) = (show d) ++ "x_" ++ (show v)


data Bound x =  x :< Double
             |  x :> Double
             |  x := Double
             deriving Show

data Constraints a = Sparse  [ Bound [Variable a] ]
                    deriving Show

data Optimization = Maximize [Double]
                  | Minimize [Double] deriving Show

type Bounds = [Bound Int]

objF :: Optimization
objF = Maximize [1, 2, 3]

st :: Constraints Int
st = Sparse [
                [(-1):#1, 1:#2, 1:#3] :< 20,
                [1:#1, (-3):#2, 1:#3] :< 30
            ]

bnds = [       (1, Just 0, Just 40),
            (2,Just 0,Nothing),
            (3, Just 0, Nothing)]

toBounds bounds varRange = F.toList $ aux bounds def
    where
        def = S.fromList [k | i <- I.range varRange, let k = (Just 0, Nothing)]
        aux [] s = s
        aux ((b,lb,ub):bs) s = aux bs (S.update (I.index varRange b) (lb,ub) s)

toConstraints constraints varRange = let (st, rhs) = toStandard constraints 0 [] [] varRange
    in (st, V.fromList rhs)
toStandard (Sparse []) _ accSt accRhs varRange = (reverse $ accSt, reverse $ accRhs)
toStandard (Sparse (b:bs)) rowI accSt accRhs varRange = case b of
        vars :< boundVal -> addRow vars L boundVal
        vars := boundVal -> addRow vars E boundVal
        vars :> boundVal -> addRow vars G boundVal
    where   addRow vars s boundVal = toStandard (Sparse bs) (rowI+1) (generateRow vars ++ accSt)
                                                ((s boundVal) : accRhs) varRange
            generateRow [] = []
            generateRow ((v :# i):vs) = (Row rowI, Col $ I.index varRange i, v):generateRow vs

toObj (Maximize dd) = (CPX_MAX, V.fromList dd)
toObj (Minimize dd) = (CPX_MIN, V.fromList dd)

main = do
    sol' (1,3) objF st bnds


--build varRange objective constraints bounds =
--    statusLp <- copyLp env lp objsen obj rhs sparsed (V.fromList xbnds)

cpx_ON :: CInt
cpx_ON  =  1
cpx_OFF :: Integer
cpx_OFF =  0

--sol' :: I.Ix a => (a,a) -> Optimization -> Constraints b -> [(a, Maybe Int, Maybe Int)] -> IO ()
sol' varRange objective constraints bounds = withEnv $ \env -> do
  setIntParam env CPX_PARAM_SCRIND cpx_ON
  setIntParam env CPX_PARAM_DATACHECK cpx_ON
  withLp env "testprob" $ \lp -> do
    let
        (objsen, obj) = toObj objective
        (cnstrs,rhs) = toConstraints constraints varRange
        xbnds = toBounds bounds varRange
    putStrLn $ show obj
    putStrLn $ show cnstrs
    putStrLn $ show xbnds
    statusLp <- copyLp env lp objsen obj rhs cnstrs (V.fromList xbnds)

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
