module Days.Day25 where

import Data.List.Split
import Data.Array.ST
import Control.Monad.ST
import Utility.UnionFind

type Point = [Int]

run :: String -> String
run s = 
    let points = parse s
        withIndex = zip [1..(length $ points)] points
        edges = [(ia, ib) | (ia, a) <- withIndex, (ib, b) <- withIndex, (distance a b) <= 3]
        in show $ runST $ do 
            arr <- newListArray (1, length $ points) [1..(length $ points)]
            unionFind edges arr 
            regions arr



parse :: String -> [Point]
parse = map parsePoint . lines 
  where parsePoint = map read . splitOn ","

distance :: Point -> Point -> Int
distance a b = sum . map (\(x, y) -> abs (x - y)) $ zip a b