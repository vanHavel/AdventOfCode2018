module Days.Day9(run) where

import Data.Map (Map)
import qualified Data.Map as Map

-- data structure
data Circle = Circle [Int] [Int] deriving (Show)

top :: Circle -> Int
top (Circle xs []) = top (Circle [] (reverse xs))
top (Circle _ (y:_)) = y

clockwise :: Circle -> Circle
clockwise (Circle xs []) = clockwise $ Circle [] (reverse xs)
clockwise (Circle xs (y:ys)) = Circle (y:xs) ys

counterclockwise :: Circle -> Circle
counterclockwise = flipCircle . clockwise . flipCircle
    where flipCircle (Circle xs ys) = (flip Circle) xs ys

delete :: Circle -> Circle
delete (Circle xs []) = delete (Circle [] (reverse xs))
delete (Circle xs (y:ys)) = Circle xs ys

insert :: Int -> Circle -> Circle
insert i (Circle xs ys) = Circle xs (i:ys)

-- solution
run :: String -> String
run s = let (players, marbles) = parse s
          in show (maxScore $ play players 0 marbles) ++ ", " ++ show (maxScore $ play players 0 (100 * marbles))

parse :: String -> (Int, Int)
parse s = (read $ words s !! 0, read $ words s !! 6)

maxScore :: (Circle, Map Int Int) -> Int
maxScore = maximum . Map.elems . snd

fpow :: Int -> (a -> a) -> (a -> a)
fpow n f = foldr (.) id $ replicate n f

play :: Int -> Int -> Int -> (Circle, Map Int Int)
play players player 0 = (Circle [] [0], Map.empty)
play players player marbles | marbles `mod` 23 == 0 = 
    (delete $ fpow 7 counterclockwise $ recCircle, Map.insertWith (+) player (marbles + (top $ fpow 7 counterclockwise $ recCircle)) recScore)
                            | otherwise = (insert marbles $ clockwise $ clockwise recCircle, recScore)
    where (recCircle, recScore) = play players ((succ player) `mod` players) (pred marbles)