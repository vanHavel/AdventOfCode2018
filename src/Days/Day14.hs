module Days.Day14(run) where

import Data.Array.ST
import Control.Monad.ST

update :: STArray s Int Int -> (Int, Int, Int) -> ST s (Int, Int, Int)
update recipeValues (elve1, elve2, recipeCount) = do 
    value1 <- readArray recipeValues elve1
    value2 <- readArray recipeValues elve2 
    let newValue = value1 + value2
    let newRecipeCount = if newValue < 10 then recipeCount + 1 else recipeCount + 2
    if newValue < 10
        then do
            writeArray recipeValues recipeCount newValue
        else do 
            writeArray recipeValues recipeCount 1
            writeArray recipeValues (recipeCount + 1) (newValue `mod` 10)
    let newElve1 = (elve1 + value1 + 1) `mod` newRecipeCount
    let newElve2 = (elve2 + value2 + 1) `mod` newRecipeCount
    return (newElve1, newElve2, newRecipeCount)

runUntil :: Int -> STArray s Int Int -> (Int, Int, Int) -> ST s [Int]
runUntil n arr input = do
    (elve1, elve2, updated) <- update arr input
    if updated >= n 
        then do 
            vals <- mapM (readArray arr) [1..(n - 1)]
            return vals
        else runUntil n arr (elve1, elve2, updated)

part1 :: String -> String
part1 s = let required = (read s) + 10 
              in runST $ do 
                arr <- newListArray (0, required + 50) $ [3, 7] ++ [0 | _ <- [0..(required + 48)]]
                vals <- runUntil required arr (0, 1, 2)
                return $ concatMap show $ take 10 $ reverse $ vals

part2 :: String -> Int
part2 s = let sequence = map (\c -> read [c]) s  
              required = 100000000
                in runST $ do 
                    arr <- newListArray (0, required + 50) $ [3, 7] ++ [0 | _ <- [0..(required + 48)]]
                    vals <- runUntil required arr (0, 1, 2)
                    return $ firstOccurrence sequence vals 1

firstOccurrence :: [Int] -> [Int] -> Int -> Int
firstOccurrence sequence vals counter = 
    let n = length sequence in
        if (take n vals == sequence)
            then counter
            else firstOccurrence sequence (tail vals) (succ counter)

run :: String -> String
run s = (part1 s) ++ ", " ++ (show $ part2 s)