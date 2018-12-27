module Days.Day23(run) where 

import Data.List.Split
import Data.Ord
import Data.List hiding (insert)
import Data.PQueue.Max hiding (filter)
import Debug.Trace

type Position = (Int, Int, Int)
data Bot = Bot {
    pos :: Position,
    radius :: Int
} deriving (Show, Eq)
data Cube = Cube {
  corner :: Position,
  dim :: Int
} deriving (Show, Eq)

originDist :: Cube -> Int 
originDist cube = if insideCube cube (0, 0, 0) then 0 else cubeDistance cube (0, 0, 0)
instance Ord Cube where
  compare c1 c2 = compare (- originDist c1) (- originDist c2)

run :: String -> String
run s = 
    let bots = parse s in 
        show (inStrongestRange bots) ++ ", " ++ show (distance (0, 0, 0) (solution bots))
          where solution bots = bestPos bots (singleton (length bots, initialCube))
                initialCube = Cube (-big, -big, -big) (2 * big)
                big = 134217728

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

splitCube :: Cube -> [Cube]
splitCube Cube{corner=(x, y, z), dim=s} = 
  let m = div s 2
    in [
      Cube (x, y, z) m,
      Cube (x + s - m, y, z) m,
      Cube (x, y + s - m, z) m,
      Cube (x, y, z + s - m) m,
      Cube (x + s - m, y + s - m, z) m,
      Cube (x + s - m, y, z + s - m) m,
      Cube (x, y + s - m, z + s - m) m,
      Cube (x + s - m, y + s - m, z + s - m) m
      ]
      
cubeBots :: Cube -> [Bot] -> Int 
cubeBots cube bots = length [bot | bot <- bots, cubeInRange cube bot]

cubeInRange :: Cube -> Bot -> Bool
cubeInRange cube@Cube{corner=(x, y, z), dim=s} Bot{pos=botPos, radius=r} = 
  insideCube cube botPos || cubeDistance cube botPos <= r

cubeDistance :: Cube -> Position -> Int 
cubeDistance Cube{corner=(x, y, z), dim=s} (bx, by, bz) = xdist + ydist + zdist
      where xdist = if x <= bx && bx <= x + s then 0 else minimum [abs (x - bx), abs (x + s - bx)]
            ydist = if y <= by && by <= y + s then 0 else minimum [abs (y - by), abs (y + s - by)]
            zdist = if z <= bz && bz <= z + s then 0 else minimum [abs (z - bz), abs (z + s - bz)]

insideCube :: Cube -> Position -> Bool
insideCube Cube{corner=(x, y, z), dim=s} (bx, by, bz) = 
  x <= bx && bx <= x + s && y <= by && by <= y + s && z <= bz && bz <= z + s

bestPos :: [Bot] -> MaxQueue (Int, Cube) -> Position
bestPos bots queue = 
  let (count, cube) = findMax queue in 
    trace (show (count, cube)) $ if dim cube == 0
      then corner cube
      else bestPos bots $ foldr insert (deleteMax queue) [(cubeBots c bots, c) | c <- splitCube cube]