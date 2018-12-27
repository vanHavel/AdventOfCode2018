module Days.Day15(run) where

import qualified Data.Map as Map 
import Data.Map (Map)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Array as Array 
import Data.Array (Array)
import Data.Ord
import Data.List
import Control.Monad.State
import Debug.Trace

type Queue = Seq
type Position = (Int, Int)
type Grid = Array Position Char

data BFSState = BFSState {
    parents :: Map Position Position,
    distances :: Map Position Int
} deriving (Eq, Show)

data Unit = Unit {
    isGood :: Bool,
    pos :: Position,
    initialPos :: Position,
    power :: Int,
    hp :: Int
} deriving (Eq, Show)

run :: String -> String
run s = let rawGrid = parse s 
            originalUnits = extractUnits rawGrid 3
            grid = cleanGrid rawGrid 
            part1 = outcome $ runState (fightUntilVictory grid 0) originalUnits 
            part2 = outcome $ runUntilFlawlessVictory rawGrid 4
              in show part1 ++ ", " ++ show part2

runUntilFlawlessVictory :: Grid -> Int -> (Int, [Unit])
runUntilFlawlessVictory grid power = 
    let initialUnits = extractUnits grid power
        (roundCount, finalUnits) = runState (fightUntilVictory (cleanGrid grid) 0) initialUnits
      in if goodCount initialUnits == goodCount finalUnits 
            then (roundCount, finalUnits)
            else runUntilFlawlessVictory grid (succ power)

goodCount :: [Unit] -> Int
goodCount = length . filter isGood

outcome :: (Int, [Unit]) -> Int 
outcome (roundCount, finalUnits) = (pred roundCount) * (sum $ map hp finalUnits)

parse :: String -> Grid 
parse s = 
    let ls = lines s
        ymax = length ls 
        xmax = length $ head ls 
          in Array.listArray ((1, 1), (ymax, xmax)) (concat ls)

cleanGrid :: Grid -> Grid 
cleanGrid = fmap (\c -> if elem c "EG" then '.' else c)

extractUnits :: Grid -> Int -> [Unit]
extractUnits grid elfPower = 
    [Unit{hp=200, isGood=(c == 'E'), pos=p, initialPos=p, power=if c == 'E' then elfPower else 3} | (p, c) <- Array.assocs grid, elem c "EG"]

fightUntilVictory :: Grid -> Int -> State [Unit] Int
fightUntilVictory grid roundCount = do 
    units <- get
    if all isGood units || all (not . isGood) units 
        then return roundCount 
        else do 
            runRound grid
            fightUntilVictory grid (succ roundCount) 

runRound :: Grid -> State [Unit] ()
runRound grid = do 
    units <- sortBy (comparing pos) <$> get
    mapM_ (turn grid) units

turn :: Grid -> Unit -> State [Unit] ()
turn grid unit = do 
    updatedUnit <- safeHead <$> filter (\u -> initialPos u == initialPos unit) <$> get
    case updatedUnit of
        Nothing -> return () 
        Just theUnit -> do
            decision <- decideMove grid theUnit
            case decision of
                Nothing -> combat theUnit
                Just pos -> do 
                    movedUnit <- doMove theUnit pos 
                    combat movedUnit

combat :: Unit -> State [Unit] ()
combat unit = do 
    best <- bestTarget unit 
    case best of 
        Nothing -> return () 
        Just target -> damage target (power unit)

damage :: Unit -> Int -> State [Unit] () 
damage unit pow = do 
    otherUnits <- filter (/= unit) <$> get 
    if hp unit <= pow
        then put otherUnits 
        else put $ (unit{hp=hp unit - pow}):otherUnits


doMove :: Unit -> Position -> State [Unit] Unit
doMove unit p = do 
    otherUnits <- filter (/= unit) <$> get  
    let updatedUnit = unit{pos=p}
    put $ updatedUnit:otherUnits
    return updatedUnit

decideMove :: Grid -> Unit -> State [Unit] (Maybe Position)
decideMove grid unit@Unit{pos=p} = do 
    best <- bestTarget unit
    case best of
        Just target -> return $ Nothing
        Nothing -> do 
            units <- get
            let ((), BFSState{parents=par, distances=dist}) = 
                         runState (bfs grid units (Seq.singleton p)) (BFSState{parents=Map.empty, distances=Map.singleton p 0})
            goal <- selectGoal unit dist
            case goal of 
                Nothing -> return Nothing 
                Just aGoal -> do 
                    let path = reconstructPath par aGoal 
                    return $ Just (Seq.index path 1)

bestTarget :: Unit -> State [Unit] (Maybe Unit)
bestTarget unit = do 
    units <- get 
    return $ safeHead $ sortBy (comparing $ \t -> (hp t, pos t)) [target | target <- units, pos target `elem` (adjacent $ pos unit), isGood target /= isGood unit]

isTarget :: Unit -> Unit -> Bool 
isTarget u1 u2 = (isGood u1 /= isGood u2)

selectGoal :: Unit -> Map Position Int -> State [Unit] (Maybe Position)
selectGoal unit dist = do 
    units <- get
    let enemyPoss = [pos u | u <- units, isGood u /= isGood unit]
        rangePos = concatMap adjacent enemyPoss
        reachableSquaresNearEnemies = Map.filterWithKey (\p _ -> p `elem` rangePos) dist 
    if Map.null reachableSquaresNearEnemies
        then return Nothing 
        else do 
            let minDist = minimum (Map.elems reachableSquaresNearEnemies) 
                bestSquares = Map.filter (== minDist) reachableSquaresNearEnemies
                goal = Just $ head $ Map.keys bestSquares
                  in return goal

reconstructPath :: Map Position Position -> Position -> Seq Position
reconstructPath parents pos = 
    case Map.lookup pos parents of 
        Nothing -> Seq.singleton pos 
        Just parent -> (reconstructPath parents parent) Seq.|> pos

safeHead :: [a] -> Maybe a 
safeHead [] = Nothing 
safeHead (a:_) = Just a

adjacent :: Position -> [Position]
adjacent (y, x) = [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]

bfs :: Grid -> [Unit] -> Queue Position -> State BFSState ()
bfs _ _ Seq.Empty = return ()
bfs grid units (pos@(y, x) Seq.:<| rest) = do 
    s <- get 
    let d = distances s Map.! pos
        sucs = filter (\suc -> accessible grid units suc && not (Map.member suc (distances s))) (adjacent pos)
        newDistances = foldr (\(k, v) -> Map.insert k v) (distances s) [(suc, (d + 1)) | suc <- sucs]
        newParents = foldr (\(k, v) -> Map.insert k v) (parents s) [(suc, pos) | suc <- sucs]
    put s{distances=newDistances, parents=newParents}
    bfs grid units (rest Seq.>< (Seq.fromList sucs))
    
accessible :: Grid -> [Unit] -> Position -> Bool 
accessible grid units (y, x) = 
    let ((ymin, xmin), (ymax, xmax)) = Array.bounds grid 
        unitPoss = map pos units
      in ymin <= y && y <= ymax && xmin <= x && x <= xmax && grid Array.! (y, x) == '.' && not (elem (y, x) unitPoss)