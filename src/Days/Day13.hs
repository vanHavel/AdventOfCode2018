module Days.Day13(run) where

import Data.Array
import Data.List

-- types and utilities
data Move = LeftTurn | Straight | RightTurn deriving (Eq, Ord, Show)
data Direction = N | E | S | W deriving (Eq, Ord, Show)
type Position = (Int, Int)
type Grid = Array Position Char

data Cart = Cart {
    position :: Position,
    direction :: Direction,
    nextMove :: Move
} deriving (Eq, Ord, Show)

turnLeft :: Direction -> Direction
turnLeft N = W
turnLeft W = S
turnLeft S = E
turnLeft E = N

turnRight :: Direction -> Direction
turnRight N = E
turnRight E = S
turnRight S = W
turnRight W = N

fromChar :: Char -> Direction
fromChar 'v' = S
fromChar '<' = W
fromChar '>' = E
fromChar '^' = N

cycleMove :: Move -> Move
cycleMove LeftTurn = Straight
cycleMove Straight = RightTurn
cycleMove RightTurn = LeftTurn

isCrossing :: Char -> Bool
isCrossing '+' = True
isCrossing _ = False

nextPosition :: Cart -> Position
nextPosition Cart{position=(y, x), direction=dir} = case dir of
    N -> (y - 1, x)
    E -> (y, x + 1)
    S -> (y + 1, x)
    W -> (y, x - 1)

nextDirection :: Direction -> Char -> Move -> Direction
nextDirection dir '-' _ = dir
nextDirection dir '|' _ = dir
nextDirection N '\\' _ = W
nextDirection S '\\' _ = E
nextDirection W '\\' _ = N
nextDirection E '\\' _ = S
nextDirection N '/' _ = E
nextDirection S '/' _ = W
nextDirection W '/' _ = S
nextDirection E '/' _ = N
nextDirection dir '+' Straight = dir
nextDirection dir '+' LeftTurn = turnLeft dir
nextDirection dir '+' RightTurn = turnRight dir

moveCart :: Grid -> Cart -> Cart
moveCart grid cart@Cart{direction=dir, nextMove=nextTurn} = 
    let nextPos = nextPosition cart
        fieldType = grid ! nextPos
          in Cart{
              position=nextPos, 
              direction=nextDirection dir fieldType nextTurn, 
              nextMove=if isCrossing fieldType then cycleMove nextTurn else nextTurn
            } 

-- parse input
parse :: String -> (Grid, [Cart])
parse s = let baseGrid = parseGrid s 
            in (removeCarts baseGrid, extractCarts baseGrid)

parseGrid :: String -> Grid
parseGrid s = listArray ((0, 0), (height - 1, width - 1)) . concat . lines $ s
    where width = length $ head $ lines s
          height = length $ lines s

removeCarts :: Grid -> Grid
removeCarts grid = changeToTrack <$> grid
    where changeToTrack '>' = '-'
          changeToTrack '<' = '-'
          changeToTrack '^' = '|'
          changeToTrack 'v' = '|'
          changeToTrack c = c

extractCarts :: Grid -> [Cart]
extractCarts grid = let cartPositions = filter (\i -> (grid ! i) `elem` "<>v^") (indices grid)
                          in [Cart{position=pos, direction=fromChar (grid ! pos), nextMove=LeftTurn} | pos <- cartPositions] 

-- run simulation
run :: String -> String
run s = let (grid, carts) = parse s in
    show (simulateUntilCollision grid carts []) ++ ", " ++ show (simulateUntilOneLeft grid carts [])

simulateUntilCollision :: Grid -> [Cart] -> [Cart] -> Position
simulateUntilCollision grid movedCarts [] = simulateUntilCollision grid [] (sort movedCarts)
simulateUntilCollision grid movedCarts (cart:nextCarts) = 
    let otherCartPositions = map position (movedCarts ++ nextCarts)
      in if (nextPosition cart) `elem` otherCartPositions 
           then nextPosition cart 
           else simulateUntilCollision grid ((moveCart grid cart):movedCarts) nextCarts

simulateUntilOneLeft :: Grid -> [Cart] -> [Cart] -> Position
simulateUntilOneLeft grid [lastCart] [] = position lastCart
simulateUntilOneLeft grid movedCarts [] = simulateUntilOneLeft grid [] (sort movedCarts)
simulateUntilOneLeft grid movedCarts (cart:nextCarts) = 
    let otherCarts = movedCarts ++ nextCarts
        nextPos = nextPosition cart
        hitCarts = filter (\cart -> position cart == nextPos) otherCarts
      in case hitCarts of
        [] -> simulateUntilOneLeft grid ((moveCart grid cart):movedCarts) nextCarts
        [hitCart] -> simulateUntilOneLeft grid (filter (/= hitCart) movedCarts) (filter (/= hitCart) nextCarts)