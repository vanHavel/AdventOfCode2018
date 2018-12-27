module Days.Day16(run) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map, (!))
import Data.Bits
import Data.Bifunctor
import Debug.Trace
-- types and utils
type Registers = Map Int Int

data Opcode = 
    ADDR | ADDI |
    MULR | MULI |
    BANR | BANI |
    BORR | BORI |
    SETR | SETI |
    GTIR | GTRI | GTRR |
    EQIR | EQRI | EQRR
  deriving (Eq, Enum, Bounded, Show)

allOpcodes :: [Opcode]
allOpcodes = [minBound..maxBound]

data Command = Command Opcode Int Int Int deriving (Show)

runCommand :: Command -> Registers -> Registers
runCommand (Command opcode a b c) regs = case opcode of
    ADDR -> Map.insert c ((regs ! a) + (regs ! b)) regs
    ADDI -> Map.insert c ((regs ! a) + b) regs
    MULR -> Map.insert c ((regs ! a) * (regs ! b)) regs
    MULI -> Map.insert c ((regs ! a) * b) regs 
    BANR -> Map.insert c ((regs ! a) .&. (regs ! b)) regs
    BANI -> Map.insert c ((regs ! a) .&. b) regs
    BORR -> Map.insert c ((regs ! a) .|. (regs ! b)) regs
    BORI -> Map.insert c ((regs ! a) .|. b) regs
    SETR -> Map.insert c (regs ! a) regs
    SETI -> Map.insert c a regs
    GTIR -> Map.insert c (if a > regs ! b then 1 else 0) regs
    GTRI -> Map.insert c (if regs ! a > b then 1 else 0) regs
    GTRR -> Map.insert c (if regs ! a > regs ! b then 1 else 0) regs
    EQIR -> Map.insert c (if a == regs ! b then 1 else 0) regs
    EQRI -> Map.insert c (if regs ! a == b then 1 else 0) regs
    EQRR -> Map.insert c (if regs ! a == regs ! b then 1 else 0) regs

run :: String -> String
run s = let (input1, input2) = parse s 
              in show (part1 input1) ++ ", " ++ show (part2 input2)

-- parse input
parse :: String -> ([(Registers, [Int], Registers)], [[Int]])
parse = bimap parse1 parse2 . splitInput [] 

splitInput :: String -> String -> (String, String)
splitInput seen ('\n':'\n':'\n':'\n':rest) = (reverse $ '\n':'\n':seen, rest)
splitInput seen (c:cs) = splitInput (c:seen) cs

parse1 :: String -> [(Registers, [Int], Registers)]
parse1 = parseLines . lines where
    parseLines [] = []
    parseLines (l1:l2:l3:[]:rest) = (parseRegisters $ tail $ words l1, map read $ words l2, parseRegisters $ tail $ words l3):parseLines rest
    parseLines a = error (show a)

parseRegisters :: [String] -> Registers
parseRegisters = Map.fromList . zip [0..3] . map read . map (filter (`elem` ' ':['0'..'9']))

parse2 :: String -> [[Int]]
parse2 = map (map read . words) . lines

-- part 1
part1 :: [(Registers, [Int], Registers)] -> Int
part1 = length . filter (\a -> length [opcode | opcode <- allOpcodes, works a opcode] >= 3)

works :: (Registers, [Int], Registers) -> Opcode -> Bool
works (regs1, [_, a, b, c], regs2) opcode = runCommand (Command opcode a b c) regs1 == regs2

-- part 2
part2 :: [[Int]] -> Map Int Int
part2 nums = let commands = map (\[i, a, b, c] -> Command (numberToOpcode ! i) a b c) nums 
                 result = foldl (flip runCommand) (Map.fromList $ zip [0..3] [0, 0, 0, 0]) commands
                   in result

opcodeOptions :: [(Registers, [Int], Registers)] -> [(Int, Opcode)]
opcodeOptions samples = [
    (i, opcode) | i <- [0..15], opcode <- allOpcodes, all (\(r1, is, r2) -> head is /= i || works (r1, is, r2) opcode) samples]

numberToOpcode :: Map Int Opcode
numberToOpcode = Map.fromList [
    (0, BANR), (1, MULI), (2, BORI), (3, SETR), 
    (4, ADDI), (5, EQRR), (6, GTRI), (7, GTIR), 
    (8, BORR), (9, EQRI), (10, BANI), (11, ADDR), 
    (12, EQIR), (13, MULR), (14, SETI), (15, GTRR)
    ]