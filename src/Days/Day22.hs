{-# LANGUAGE BangPatterns #-}
module Days.Day22(run) where

import Control.Monad.State
import Data.Array
import qualified Data.Set as Set
import Data.Set (Set)
import Utility.AStar
import Debug.Trace

type Position = (Int, Int)

run :: String -> String
run s = 
    let (depth, target@(y, x)) = parse s 
        smallGrid = makeGrid depth target ((0, 0), target)
        largeGrid = makeGrid depth target ((0, 0), (y + 800, x + 800))
      in show (riskLevel smallGrid) ++ ", " ++ show (searchTarget largeGrid target)

parse :: String -> (Int, Position)
parse s = let [l1, l2] = lines s 
              (x, ',':y) = span (/= ',') $ (words l2) !! 1 in 
    (read $ (words l1) !! 1, (read y, read x))

makeGrid :: Int -> Position -> (Position, Position) -> Array Position Int
makeGrid depth target bounds = (`mod` 3) <$> erosionGrid 
  where erosionGrid = listArray bounds [compute (y, x) | y <- [0..(fst $ snd bounds)], x <- [0..(snd $ snd bounds)]]
        compute (0, 0) = erode 0
        compute pos | pos == target = erode 0
        compute (0, x) = erode $ x * 16807
        compute (y, 0) = erode $ y * 48271
        compute (y, x) = erode $ (erosionGrid ! (y - 1, x)) * (erosionGrid ! (y, x - 1))
        erode i = (i + depth) `mod` 20183

riskLevel :: Array Position Int -> Int 
riskLevel = sum . elems

data Tool = ClimbingGear | Torch | Neither 
  deriving (Eq, Ord, Show)
data CaveState = CaveState {
    pos :: !Position,
    tool :: !Tool
} deriving (Eq, Ord, Show)

allTools :: [Tool]
allTools = [ClimbingGear, Torch, Neither]  

searchTarget :: Array Position Int -> Position -> Int
searchTarget grid target = aStarSearch initial final (manhattanHeuristic target) (succFun grid)
  where initial = CaveState{pos=(0, 0), tool=Torch}
        final = CaveState{pos=target, tool=Torch}

manhattanHeuristic :: Position -> CaveState -> Int 
manhattanHeuristic (!ty, !tx) CaveState{pos=(!y, !x)} = abs (y - ty) + abs (x - tx)

succFun :: Array Position Int -> CaveState -> Set (CaveState, Int)
succFun !grid CaveState{pos=thePos, tool=theTool} = 
    let succPoss = getSuccPoss thePos 
        moveStates = zip [CaveState{pos=newPos, tool=theTool} | newPos <- succPoss, possible grid newPos theTool] (repeat 1)
        toolStates = [(CaveState{pos=thePos, tool=otherTool theTool (grid ! thePos)}, 7)]
      in Set.fromList $ moveStates ++ toolStates

getSuccPoss :: Position -> [Position]
getSuccPoss (0, 0) = [(1, 0), (0, 1)]
getSuccPoss (0, !x) = [(1, x), (0, x + 1), (0, x - 1)]
getSuccPoss (!y, 0) = [(y, 1), (y + 1, 0), (y - 1, 0)]
getSuccPoss (!y, !x) = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]

possible :: Array Position Int -> Position -> Tool -> Bool
possible grid pos tool = case grid ! pos of
    0 -> tool /= Neither
    1 -> tool /= Torch
    2 -> tool /= ClimbingGear

otherTool :: Tool -> Int -> Tool
otherTool ClimbingGear 0 = Torch 
otherTool Torch 0 = ClimbingGear 
otherTool ClimbingGear 1 = Neither
otherTool Neither 1 = ClimbingGear 
otherTool Torch 2 = Neither
otherTool Neither 2 = Torch