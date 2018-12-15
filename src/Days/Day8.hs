module Days.Day8(run) where

data Tree a = Node a [Tree a]

run :: String -> String
run s = let tree = buildTree (map read $ words s) in 
          show (sumTree tree) ++ ", " ++ show (evalTree tree)

buildTree :: [Int] -> Tree [Int]
buildTree = head . fst . buildTrees 1

buildTrees :: Int -> [Int] -> ([Tree [Int]], [Int])
buildTrees 0 tokens = ([], tokens)
buildTrees n (children:metacount:tokens) = 
    let (childTrees, leftover) = buildTrees children tokens
        (metadata, rest) = splitAt metacount leftover
        firstTree = Node metadata childTrees
        (nextTrees, lastRest) = buildTrees (pred n) rest 
          in ((firstTree:nextTrees), lastRest)

sumTree :: Tree [Int] -> Int
sumTree (Node is children) = sum is + sum (map sumTree children)

evalTree :: Tree [Int] -> Int
evalTree (Node metadata []) = sum metadata
evalTree (Node metadata children) = 
    sum $ map (\i -> evalTree (children !! pred i)) $ filter (`elem` [1..(length children)]) metadata

