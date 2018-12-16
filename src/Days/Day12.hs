module Days.Day12(run) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

type Rules = Map String Char

run :: String -> String
run s = let (initial, rules) = parse s 
          in show (runGens 20 rules initial) ++ ", " ++ show ((runGens 300 rules initial) + 42 * (50000000000 - 300)) 

parse :: String -> (String, Rules)
parse s = let (first:_:rest) = lines s
              initial = (words first) !! 2
              rules = Map.fromList [(words line !! 0, head $ words line !! 2) | line <- rest]
                in (initial, rules)

fpow :: Int -> (a -> a) -> (a -> a)
fpow n f = foldr (.) id $ replicate n f

runGens :: Int -> Rules -> String -> Integer
runGens i rules = fromIntegral . sumPresent (-2 * i) . fpow i (runGen rules)

part2 :: Rules -> String -> [String]
part2 rules = iterate (runGen rules)

runGen :: Rules -> String -> String
runGen rules = process rules . extend

extend :: String -> String
extend s = "...." ++ s ++ "...."

process :: Rules -> String -> String
process rules s | length s < 5 = ""
                | otherwise = (Map.findWithDefault '.' (take 5 s) rules):(process rules $ tail s)

sumPresent :: Int -> String -> Int
sumPresent _ "" = 0
sumPresent i ('#':cs) = i + sumPresent (succ i) cs
sumPresent i ('.':cs) = sumPresent (succ i) cs