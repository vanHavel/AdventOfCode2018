module Days.Day23 where 

import Data.List.Split
import Data.Ord
import Data.List
import Debug.Trace

type Position = (Int, Int, Int)
data Bot = Bot {
    pos :: Position,
    radius :: Int
} deriving (Show, Eq)

run :: String -> String
run s = 
    let bots = parse s in 
        show (inStrongestRange bots)

parse :: String -> [Bot]
parse = map parseBot . lines 

parseBot :: String -> Bot
parseBot line = 
    let [x, y, z, r] = map read $ splitOn "," $ filter (`elem` ',':'-':['0'..'9']) line
      in Bot {pos=(x, y, z), radius=r}

inStrongestRange :: [Bot] -> Int 
inStrongestRange bots = length $ [bot | bot <- bots, inRange strongestBot (pos bot)]
  where strongestBot = maximumBy (comparing radius) bots

inRange :: Bot -> Position -> Bool
inRange bot p = distance (pos bot) p <= (radius bot)

distance :: Position -> Position -> Int 
distance (a, b, c) (d, e, f) = (abs $ a - d) + (abs $ b - e) + (abs $ c - f)