module Days.Day11 where

import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as Map
import Data.List
import Data.Ord

type Position = (Int, Int)

run :: String -> String
run s = show (maxPower $ read s) ++ ", " ++ show (maxDpPower $ read s)

power :: Int -> Position -> Int
power serial (x, y) = (((((x + 10) * y + serial) * (x + 10)) `div` 100) `mod` 10) - 5

gridPower :: Int -> Position -> Int
gridPower serial (x, y) = sum [power serial (x+i, y+j) | i <- [0..2], j <- [0..2]]

maxPower :: Int -> Position
maxPower serial = snd $ maximum [(gridPower serial (x, y), (x, y)) | x <- [1..298], y <- [1..298]]

dpPowers :: Int -> Map (Position, Int) Int
dpPowers serial = theMap 
    where theMap = Map.fromList [(((x, y), size), go (x, y) size) | x <- [1..300], y <- [1..300], size <- [1..(minimum [301-x, 301-y])]]
          go (x, y) 1 = power serial (x, y) 
          go (x, y) 2 = power serial (x, y) + power serial (x + 1, y) + power serial (x, y + 1) + power serial (x + 1, y + 1) 
          go (x, y) n = theMap ! ((x, y), n - 1) + theMap ! ((x + 1, y + 1), n - 1) - theMap ! ((x + 1, y + 1), n - 2) + power serial (x + n - 1, y) + power serial (x, y + n - 1) 

maxDpPower :: Int -> (Position, Int)
maxDpPower serial = fst $ maximumBy (comparing snd) $ Map.assocs $ dpPowers serial