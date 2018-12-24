module Days.Day21(run) where

import Data.Array
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Bits
import Control.Monad.Identity

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

data Command = Command Opcode Int Int Int deriving (Show)

type Program = Array Int Command

runCommand :: Command -> Registers -> Registers
runCommand (Command opcode a b c) regs = case opcode of
    ADDR -> Map.insert c ((regs Map.! a) + (regs Map.! b)) regs
    ADDI -> Map.insert c ((regs Map.! a) + b) regs
    MULR -> Map.insert c ((regs Map.! a) * (regs Map.! b)) regs
    MULI -> Map.insert c ((regs Map.! a) * b) regs 
    BANR -> Map.insert c ((regs Map.! a) .&. (regs Map.! b)) regs
    BANI -> Map.insert c ((regs Map.! a) .&. b) regs
    BORR -> Map.insert c ((regs Map.! a) .|. (regs Map.! b)) regs
    BORI -> Map.insert c ((regs Map.! a) .|. b) regs
    SETR -> Map.insert c (regs Map.! a) regs
    SETI -> Map.insert c a regs
    GTIR -> Map.insert c (if a > regs Map.! b then 1 else 0) regs
    GTRI -> Map.insert c (if regs Map.! a > b then 1 else 0) regs
    GTRR -> Map.insert c (if regs Map.! a > regs Map.! b then 1 else 0) regs
    EQIR -> Map.insert c (if a == regs Map.! b then 1 else 0) regs
    EQRI -> Map.insert c (if regs Map.! a == b then 1 else 0) regs
    EQRR -> Map.insert c (if regs Map.! a == regs Map.! b then 1 else 0) regs

runProgram :: Program -> Int -> Registers -> Int -> Int
runProgram _ _ regs 28 = regs Map.! 5
runProgram program ipreg regs ip | ip < (fst $ bounds program) || ip > (snd $ bounds program) = regs Map.! 0
                                    | otherwise = runProgram program ipreg updatedRegs (updatedRegs Map.! ipreg) 
                                        where updatedRegs = Map.adjust succ ipreg (runCommand (program Data.Array.! ip) regs)

run :: String -> String
run s = let (ipreg, program) = parse s 
            regs = Map.fromList $ zip [0..5] $ repeat 0 in
               show (runProgram program ipreg regs 0) ++ ", " ++ show (runIdentity $ generateUntilRepeat Set.empty 0)


parse :: String -> (Int, Program)
parse s = let (first:rest) = lines s in
            (read $ (words first) !! 1, listArray (0, pred $ length rest) (map parseCommand rest))

parseCommand :: String -> Command
parseCommand s = let [opc, a, b, c] = words s in
                   Command (parseOpcode opc) (read a) (read b) (read c)

parseOpcode :: String -> Opcode
parseOpcode "addr" = ADDR
parseOpcode "addi" = ADDI
parseOpcode "mulr" = MULR
parseOpcode "muli" = MULI
parseOpcode "banr" = BANR
parseOpcode "bani" = BANI
parseOpcode "borr" = BORR
parseOpcode "bori" = BORI
parseOpcode "setr" = SETR
parseOpcode "seti" = SETI
parseOpcode "gtir" = GTIR
parseOpcode "gtri" = GTRI
parseOpcode "gtrr" = GTRR
parseOpcode "eqri" = EQRI
parseOpcode "eqir" = EQIR
parseOpcode "eqrr" = EQRR

generator :: Integer -> Identity Integer
generator i = do
    r4 <- return $ i .|. 65536
    r5 <- return 3935295
    subGenerator r4 r5

subGenerator :: Integer -> Integer -> Identity Integer
subGenerator r4 r5 = do
    r2 <- return $ r4 .&. 255
    r5 <- return $ r5 + r2
    r5 <- return $ r5 .&. 16777215
    r5 <- return $ r5 * 65899
    r5 <- return $ r5 .&. 16777215
    if r4 > 255 
        then subGenerator (div r4 256) r5 
        else return r5

generateUntilRepeat :: Set Integer -> Integer -> Identity Integer
generateUntilRepeat seen current = do
    next <- generator current
    if Set.member next seen 
        then return current
        else generateUntilRepeat (Set.insert next seen) next