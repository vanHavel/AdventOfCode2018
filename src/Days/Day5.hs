module Days.Day5(run) where

import Data.Char

run :: String -> String
run s = show (length $ reduce (init s) []) ++ ", " ++ show (bestLength $ init s)

reduce :: String -> String -> String
reduce [] ys = ys
reduce (x:xs) [] = reduce xs [x]
reduce (x:xs) (y:ys) | x /= y && toLower x == toLower y = reduce xs ys
                     | otherwise                        = reduce xs (x:y:ys)

bestLength :: String -> Int
bestLength s = minimum [length $ reduce (filter (\c -> toLower c /= a) s) [] | a <- ['a'..'z']]