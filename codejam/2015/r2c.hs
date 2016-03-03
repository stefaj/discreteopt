import Data.List
import System.Environment
import Data.Text (pack, unpack, splitOn)
import Control.Monad

simScore xs ys = (fromIntegral $ simScore' xs ys) / (fromIntegral $ minimum [length xs, length ys])
    where   x2 = fromIntegral $ (length xs) * (length xs)
            y2 = fromIntegral $ (length ys) * (length ys)
simScore' [] _ = 0
simScore' (x:xs) ys = (if x `elem` ys then 0 else 1) + simScore' xs ys

combos sentences = filter (\(a,b) -> simScore a b > 0.1) $ liftM2 (,) sentences sentences

getAns sentences = minimum $ map (\(a,b) -> simScore' a b) $ combos sentences

showAns [] _ = []
showAns (sentences:ts) i =
    (outputAns i $ getAns sentences) : showAns ts (i+1)

linesToTestCase [] testCases = reverse $ testCases
linesToTestCase (h:hs) t = let
        n = read h
        (sentences, other) = splitAt n hs
    in linesToTestCase other (sentences:t)

main = do
    fileName <- argOrInput 0
    contents <- readFile fileName
    let (h:c) = lines contents
    let testCases = linesToTestCase c []
    let output = unlines $ showAns testCases 1
    writeFile (fileName ++ ".out") output
    putStrLn $ show $ testCases !! 0
    putStrLn $ output

argOrInput n = do
	args <- getArgs
	if length args < 1 then
		getLine
	else
		return $ args !! n

outputAns i a = "Case #" ++ (show i) ++ ": " ++ (show a)
