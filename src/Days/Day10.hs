module Days.Day10(run) where

import Data.Array

type Vector = (Int, Int)

add :: Vector -> Vector -> Vector
add (a, b) (c, d) = (a + c, b + d)

scale :: Int -> Vector -> Vector
scale i (a, b) = (i * a, i * b)

run :: String -> String
run s = let input = parse s 
          in showGrid $ makeGrid $ move 10274 input

parse :: String -> [(Vector, Vector)]
parse = map (parseLine . filter (\c -> elem c ('-':' ':['0'..'9']))) . lines where
    parseLine line = let [a, b, c, d] = map read $ words line in ((a, b), (c, d))

move :: Int -> [(Vector, Vector)] -> [Vector]
move n = map (\(vec, vel) -> add vec (scale n vel))

makeGrid :: [Vector] -> Array Vector Char
makeGrid vecs = let minx = minimum $ map fst $ vecs
                    maxx = maximum $ map fst $ vecs
                    miny = minimum $ map snd $ vecs
                    maxy = maximum $ map snd $ vecs
                      in array ((miny, minx), (maxy, maxx)) [((a, b), if (b, a) `elem` vecs then '#' else '.') | a <- [miny..maxy], b <- [minx..maxx]]

showGrid :: Array Vector Char -> String
showGrid arr = let width = (snd $ snd $ bounds arr) - (snd $ fst $ bounds arr) + 1
                 in unlines $ splitEvery width $ elems arr

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = (take n xs):(splitEvery n (drop n xs))