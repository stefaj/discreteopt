import Data.Ix as I
import qualified Data.Vector as V
import qualified Data.Sequence as S

bnds :: [(Int, Maybe Int, Maybe Int)]
bnds = [       (1, Just 0, Just 40),
            (2,Just 0,Nothing),
            (3, Just 0, Nothing)]

toBounds bounds varRange = aux bounds def
    where
        def = S.fromList [k | i <- I.range varRange, let k = (Just 0, Nothing)]
        -- aux [(Int, Maybe Int, Maybe Int)]
        aux [] s = s
        aux ((b,lb,ub):bs) s = aux bs (S.update (I.index varRange b) (lb,ub) s)
