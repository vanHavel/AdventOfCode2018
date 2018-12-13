module Days.Day7 where

import Data.Char

type Condition = (Char, Char)

run :: String -> String
run s = let conditions = parse s in
           show (path [] $ requirements conditions) ++ ", " ++ show (timedPath 5 [] $ requirementsWithTime conditions)

-- parse
parse :: String -> [Condition]
parse s = map parseCondition $ lines s

parseCondition :: String -> Condition
parseCondition line = (head . (!! 1) . words $ line, head . (!! 7) . words $ line)

-- part 1
requirements :: [Condition] -> [(Char, [Char])]
requirements conditions = [(a, map fst $ filter (\t -> snd t == a) conditions) | a <- ['A'..'Z']]

path :: [(Char, [Char])] -> [(Char, [Char])] -> String
path _ [] = []
path xs ((a, []):ys) = a:(path [] (map (\(b, bs) -> (b, filter (/=a) bs)) $ reverse xs ++ ys))
path xs (y:ys) = path (y:xs) ys

-- part 2
requirementsWithTime :: [Condition] -> [(Char, Int, Bool, [Char])]
requirementsWithTime = map (\(a, as) -> (a, 61 + (ord a - ord 'A'), False, as)) . requirements

timedPath :: Int -> [(Char, Int, Bool, [Char])] -> [(Char, Int, Bool, [Char])] -> Int
timedPath free [] [] = 0
timedPath free xs [] = let removes = freeTups xs 
                         in succ $ timedPath (free + length removes) [] (clean removes $ reverse xs)
timedPath free xs ((a, i, False, []):ys) | free > 0 = timedPath (pred free) ((a, pred i, True, []):xs) ys
                                         | otherwise = timedPath 0 ((a, i, False, []):xs) ys
timedPath free xs ((a, i, True, []):ys) = timedPath free ((a, pred i, True, []):xs) ys                             
timedPath free xs ((a, i, b, as):ys) = timedPath free ((a, i, b, as):xs) ys

freeTups :: [(Char, Int, Bool, [Char])] -> [(Char, Int, Bool, [Char])]
freeTups = filter (\(a, i, t, as) -> i == 0)

clean :: [(Char, Int, Bool, [Char])] -> [(Char, Int, Bool, [Char])] -> [(Char, Int, Bool, [Char])]
clean removes = map (\(a, i, t, as) -> (a, i, t, filter (\b -> not (b `elem` (map (\(c, _, _, _) -> c) removes))) as)) . filter (\a -> not (a `elem` removes))