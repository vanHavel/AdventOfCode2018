module Days.Day24(run) where 

import Data.Ord
import Data.List
import Debug.Trace

data AttackType = Radiation | Fire | Cold | Slashing | Bludgeoning
  deriving (Eq, Show)
data Group = Group {
    isGood :: Bool,
    unitCount :: Int,
    unitHP :: Int,
    unitDamage :: Int,
    initiative :: Int,
    attackType :: AttackType,
    weak :: [AttackType],
    immune :: [AttackType]
} deriving (Eq, Show)

testInput :: [Group]
testInput = [
  Group True 17 5390 4507 2 Fire [Radiation, Bludgeoning] [],
  Group True 989 1274 25 3 Slashing [Bludgeoning, Slashing] [Fire],
  Group False 801 4706 116 1 Bludgeoning [Radiation] [],
  Group False 4485 2961 12 4 Slashing [Fire, Cold] [Radiation]
  ]

inputGroups :: [Group]
inputGroups = [
    Group True 2743 4149 13 14 Radiation [] [],
    Group True 8829 7036 7 15 Fire [] [],
    Group True 1928 10700 50 3 Slashing [Cold] [Fire, Radiation, Slashing],
    Group True 6051 11416 15 20 Bludgeoning [] [],
    Group True 895 10235 92 10 Bludgeoning [Bludgeoning] [Slashing],
    Group True 333 1350 36 12 Radiation [] [],
    Group True 2138 8834 35 11 Cold [Bludgeoning] [],
    Group True 4325 1648 3 8 Bludgeoning [Cold, Fire] [],
    Group True 37 4133 1055 1 Radiation [] [Radiation, Slashing],
    Group True 106 3258 299 13 Cold [] [Slashing, Radiation],
    Group False 262 8499 45 6 Cold [Cold] [],
    Group False 732 47014 127 17 Bludgeoning [Cold, Bludgeoning] [],
    Group False 4765 64575 20 18 Radiation [] [],
    Group False 3621 19547 9 5 Cold [] [Radiation, Cold],
    Group False 5913 42564 14 9 Slashing [] [Radiation, Bludgeoning, Fire],
    Group False 7301 51320 11 2 Fire [Radiation, Fire] [Bludgeoning],
    Group False 3094 23713 14 19 Radiation [Slashing, Fire] [],
    Group False 412 36593 177 16 Slashing [Radiation, Bludgeoning] [],
    Group False 477 35404 146 7 Cold [] [],
    Group False 332 11780 70 4 Slashing [Fire] []
  ]

run :: String -> String
run _ = let boost = binsearch 0 100 (\i -> goodWins inputGroups i) 
            Just units1 = sum <$> map unitCount <$> fightUntilVictory inputGroups 
            Just units2 = sum <$> map unitCount <$> fightUntilVictory (boostAttack boost inputGroups)
              in show units1 ++ ", " ++ show units2

goodWins :: [Group] -> Int -> Bool
goodWins groups boost = case fightUntilVictory $ boostAttack boost groups of 
    Nothing -> False 
    Just result -> all isGood result

boostAttack :: Int -> [Group] -> [Group]
boostAttack boost = map (\g -> if isGood g then g{unitDamage = unitDamage g + boost} else g)

binsearch :: Int -> Int -> (Int -> Bool) -> Int 
binsearch min max f | min == max = min 
                    | otherwise = let mid = min + (div (max - min) 2) in 
                        if f mid 
                          then binsearch min mid f 
                          else binsearch (mid + 1) max f

fightUntilVictory :: [Group] -> Maybe [Group]
fightUntilVictory groups = 
  let afterRound = battleRound groups in 
    if all isGood afterRound || all (not . isGood) afterRound
      then Just afterRound 
      else if afterRound == groups
        then Nothing
        else fightUntilVictory afterRound

battleRound :: [Group] -> [Group]
battleRound groups = 
  let fights = allTargets (sortBy (comparing targetSpeed) groups) groups
    in allFights groups $ sortBy (comparing $ negate . initiative . fst) fights

targetSpeed :: Group -> (Int, Int)
targetSpeed group = (negate $ effectivePower group, negate $ initiative group)

allTargets :: [Group] -> [Group] -> [(Group, Group)]
allTargets [] _ = []
allTargets (g:gs) targets = case targetSelection g targets of 
  Nothing -> allTargets gs targets
  Just t -> (g, t):(allTargets gs (filter (/= t) targets))

allFights :: [Group] -> [(Group, Group)] -> [Group]
allFights groups [] = groups
allFights groups ((attacker, defender):fights) = 
  let remainingGroups = filter (/= defender) groups 
    in case fight attacker defender of 
      Nothing -> allFights remainingGroups $ filter (\(a, _) -> a /= defender) fights
      Just defender' -> defender':(allFights remainingGroups $ updatedFights defender defender' fights)
  where updatedFights _ _ [] = []
        updatedFights defender defender' ((a, d):fights) | a == defender = (defender', d):fights
                                                         | otherwise = (a, d):(updatedFights defender defender' fights) 


fight :: Group -> Group -> Maybe Group
fight attacker defender = 
  let dealtDamage = damage attacker defender
      unitsKilled = dealtDamage `div` (unitHP defender)
        in if unitsKilled >= (unitCount defender)
             then Nothing
             else Just defender{unitCount = unitCount defender - unitsKilled}

targetSelection :: Group -> [Group] -> Maybe Group
targetSelection attacker targets = mhead $ 
  sortBy (comparing $ \target -> (negate $ damage attacker target, negate $ effectivePower target, negate $ initiative target)) $ 
    filter (\t -> isGood t /= isGood attacker && damage attacker t > 0) targets
  where mhead [] = Nothing 
        mhead (target:_) = Just target

damage :: Group -> Group -> Int 
damage attacker defender = (multiplier attacker defender) * (effectivePower attacker)

multiplier :: Group -> Group -> Int 
multiplier attacker defender = 
    if (attackType attacker) `elem` (weak defender)
      then 2
    else if (attackType attacker) `elem` (immune defender)
      then 0
      else 1

effectivePower :: Group -> Int
effectivePower attacker = (unitCount attacker) * (unitDamage attacker)