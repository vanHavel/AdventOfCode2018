module Days.Day15 where

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
    hp :: Int
} deriving (Eq, Show)

run :: String -> String
run s = let rawGrid = parse s 
            units = extractUnits rawGrid 
            grid = cleanGrid grid 
            (roundCount, finalUnits) = runState (fightUntilVictory grid 0) units 
              in show ((pred roundCount) * (sum $ map hp finalUnits))

parse :: String -> Grid 
parse s = 
    let ls = lines s
        ymax = length ls 
        xmax = length $ head ls 
          in Array.listArray ((1, 1), (ymax, xmax)) (concat ls)

cleanGrid :: Grid -> Grid 
cleanGrid = fmap (\c -> if elem c "EG" then '.' else c)

extractUnits :: Grid -> [Unit]
extractUnits grid = 
    [Unit{hp=200, isGood=(c == 'E'), pos=p} | (p, c) <- Array.assocs grid, elem c "EG"]

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
turn grid unit = if hp unit <= 0
                then do 
                    otherUnits <- filter (/= unit) <$> get 
                    put otherUnits
                else do 
                    decision <- decideMove grid unit
                    case decision of
                        Nothing -> return ()
                        Just pos -> doMove unit pos 
                    combat unit

combat :: Unit -> State [Unit] ()
combat unit = do 
    best <- bestTarget unit 
    case best of 
        Nothing -> return () 
        Just target -> damage target 

damage :: Unit -> State [Unit] () 
damage unit = do 
    otherUnits <- filter (/= unit) <$> get 
    put $ (unit{hp=hp unit - 3}):otherUnits


doMove :: Unit -> Position -> State [Unit] ()
doMove unit p = do 
    otherUnits <- filter (/= unit) <$> get  
    let updatedUnit = unit{pos=p}
    put $ updatedUnit:otherUnits

decideMove :: Grid -> Unit -> State [Unit] (Maybe Position)
decideMove grid unit@Unit{pos=p} = do 
    best <- bestTarget unit
    case best of
        Just target -> return $ Nothing
        Nothing -> do 
            let ((), BFSState{parents=par, distances=dist}) = 
                         runState (bfs grid (Seq.singleton p)) (BFSState{parents=Map.empty, distances=Map.singleton p 0})
            goal <- selectGoal unit dist
            case goal of 
                Nothing -> return Nothing 
                Just aGoal -> do 
                    let path = reconstructPath par aGoal 
                    return $ Just (Seq.index path 1)

bestTarget :: Unit -> State [Unit] (Maybe Unit)
bestTarget unit = do 
    units <- get 
    return $ safeHead $ sortBy (comparing pos) [target | target <- units, pos target `elem` (adjacent $ pos unit)]

isTarget :: Unit -> Unit -> Bool 
isTarget u1 u2 = (isGood u1 /= isGood u2)

selectGoal :: Unit -> Map Position Int -> State [Unit] (Maybe Position)
selectGoal unit dist = do 
    units <- get
    let enemyPoss = [pos u | u <- units, isGood u /= isGood unit]
        reachableEnemies = Map.filterWithKey (\p _ -> p `elem` enemyPoss) dist 
    if Map.null reachableEnemies
        then return Nothing 
        else do 
            let minDist = minimum (Map.elems reachableEnemies) 
                bestEnemies = Map.filter (== minDist) reachableEnemies
            return $ Just $ head $ Map.keys bestEnemies

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

bfs :: Grid -> Queue Position -> State BFSState ()
bfs _ Seq.Empty = return ()
bfs grid (pos@(y, x) Seq.:<| rest) = do 
    s <- get 
    let d = distances s Map.! pos
        sucs = filter (\suc -> accessible grid suc && not (Map.member suc (distances s))) (adjacent pos)
        newDistances = foldr (\(k, v) -> Map.insert k v) (distances s) [(suc, (d + 1)) | suc <- sucs]
        newParents = foldr (\(k, v) -> Map.insert k v) (parents s) [(suc, pos) | suc <- sucs]
    put s{distances=newDistances, parents=newParents}
    bfs grid (rest Seq.>< (Seq.fromList sucs))
    
accessible :: Grid -> Position -> Bool 
accessible grid (y, x) = 
    let ((ymin, xmin), (ymax, xmax)) = Array.bounds grid 
      in ymin <= y && y <= ymax && xmin <= x && x <= xmax && grid Array.! (y, x) == '.'