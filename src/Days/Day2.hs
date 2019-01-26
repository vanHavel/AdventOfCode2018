module Days.Day2(run) where

import Data.List

run :: String -> String
run s = let hashes = lines s in
    show (checksum hashes) ++ ", " ++ (solution hashes)

checksum :: [String] -> Int
checksum hashes = 
    let twiceHashes  = filter (anyOccursTimes 2) hashes
        thriceHashes = filter (anyOccursTimes 3) hashes in 
            (length twiceHashes) * (length thriceHashes)

anyOccursTimes :: Int -> String -> Bool
anyOccursTimes times = elem times . occurCounts
    where occurCounts = (map length) . group . sort

solution :: [String] -> String
solution hashes = head [equalChars s t | s <- hashes, t <- hashes, s < t, (countDifferences s t) == 1]

equalChars :: String -> String -> String
equalChars s t = [a | (a, b) <- zip s t, a == b]

countDifferences :: String -> String -> Int
countDifferences s t = length [a | (a, b) <- zip s t, a /= b]