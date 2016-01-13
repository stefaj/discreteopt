import Data.String.Utils
import Data.List

-- k = 11
-- n = 4

-- items = [(8,4,0), (10,5,1), (15,8,2), (4,3,3)]

-- exhaustive s xs =
--  let   value = sum $ map fst s
--        weight = sum $ map snd s


deleteAt xs n = let (ys,zs) = splitAt n xs
                in ys ++ (tail zs)

-- subsetPerms xs = map (deleteAt xs) [0..length xs]

pick :: Int -> [a] -> [[a]]
pick 0 _ = [[]]
pick _ [] = []
pick n (x:xs) = map (x:) (pick (n-1) xs) ++ pick n xs

arrange :: Int -> [a] -> [[a]]
arrange n = concatMap permutations . pick n

subsetPerms xs = concat $  map (flip arrange xs) [0..length xs]

validSet k xs  = (sum $ map (\(a,b,c) -> b) xs) <= k

setValue xs = sum $ map (\(a,b,c) -> a) xs


getBest [] _ b = b
getBest (x:xs) m b = let xv = setValue x in
                       if xv > m then
                            getBest xs xv x
                       else getBest xs m b

quicksort (x:xs) = [b | b <- xs, b < x] ++ [x] ++ [b | b <- xs, b > x]

normalize xs n = let nm' = map (\(a,b,c) -> c) xs  in
                   map (\a -> if a `elem` nm' then 1 else 0) [0..(n-1)]

lineToItem (line,i) = let
                        formatted = replace " " ", " line
                        item' = (read ("[" ++ formatted ++ "]")) :: [Int]
                      in
                        (item' !! 0, item' !! 1, i)

main = do
  content <- readFile "knapdata/ks_19_0"
  let l = lines content
  let (n,k) = read ("(" ++ (replace " " ", " (l !! 0)) ++ ")") :: (Int, Int)
  let items = map lineToItem $ zip (tail l) [0..(n-1)]
  let t = filter (validSet k) $ subsetPerms items
  let b = getBest t 0 []
  let ans = normalize b n
  putStrLn $ show ans
