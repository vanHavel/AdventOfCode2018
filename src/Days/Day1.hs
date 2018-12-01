module Days.Day1(run) where

import qualified Data.Set as Set
import Data.Set (Set)

run :: String -> String
run s = let numbers = parse s in
    show (sum numbers) ++ ", " ++ show (firstDuplicate (cycle numbers))

parse :: String -> [Int]
parse = map parseLine . lines where 
    parseLine ('+':s) = read s 
    parseLine ('-':s) = -(read s)

firstDuplicate :: [Int] -> Int 
firstDuplicate numbers = firstDuplicate' numbers 0 Set.empty

firstDuplicate' :: [Int] -> Int -> Set Int -> Int
firstDuplicate' (i:is) acc seen | acc `Set.member` seen = acc
                                | otherwise              = firstDuplicate' is (acc + i) (Set.insert acc seen)