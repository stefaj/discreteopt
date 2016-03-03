import Data.List
import System.Environment
import Data.Text (pack, unpack, splitOn)

toDirs dir = let
        constit = map unpack $ splitOn (pack "/") $ pack dir
        aux [] acc = acc
        aux (p:ps) [] = aux ps [p]
        aux (p:ps) acc@(a:cc) = aux ps ((a ++ "/" ++ p):acc)
    in filter (/="") $ aux constit []


existing1 = ["/chicken", "/chicken/egg"]
existing2 = ["/a"]

howMany existing dir = let dirs = toDirs dir in
    (dirs, length $ filter (\a -> not $ a `elem` existing) dirs)

howManyTotal existing [] = 0
howManyTotal existing (n:ns) = let (dirs, count) = howMany existing n
    in count + howManyTotal (dirs ++ existing) ns

showAns [] _ = []
showAns ((existing,new):ts) i =
    (outputAns i $ howManyTotal existing new) : showAns ts (i+1)

linesToTestCase [] testCases = reverse $ testCases
linesToTestCase (h:hs) t = let
        [n,m] = map read $ words h
        (existing, other) = splitAt n hs
        (new, hs') = splitAt m other
    in linesToTestCase hs' ((existing,new):t)

main = do
    fileName <- argOrInput 0
    contents <- readFile fileName
    let (h:c) = lines contents
    let testCases = linesToTestCase c []
    let output = unlines $ showAns testCases 1
    writeFile (fileName ++ ".out") output
    putStrLn $ output

argOrInput n = do
	args <- getArgs
	if length args < 1 then
		getLine
	else
		return $ args !! n
