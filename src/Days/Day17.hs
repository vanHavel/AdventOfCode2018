module Days.Day17(run) where

import qualified Data.Set as Set 
import Data.Set (Set)
import Data.List.Split
import qualified Data.Array as Array
import Data.Array (Array)
import qualified Data.Array.ST as MArray
import Data.Array.ST (STArray)
import Control.Monad.ST
import Debug.Trace

type Position = (Int, Int)

run :: String -> String
run s = 
    let grid = makeGrid $ parse s 
        ymin = fst $ fst $ Array.bounds grid
        filled = runST $ do 
            arr <- MArray.thaw grid
            fillFromPosition arr (ymin, 500)
            frozen <- MArray.freeze arr
            return frozen
          in show (waterCount filled "|~") ++ "," ++ show (waterCount filled "~")


waterCount :: Array Position Char -> String -> Int 
waterCount grid chars = 
    let ((ymin, xmin), (ymax, xmax)) = Array.bounds grid 
        vals = [c | ((y, x), c) <- Array.assocs grid, y > ymin, elem c chars]
          in length vals

fillFromPosition :: STArray s Position Char -> Position -> ST s ()
fillFromPosition grid pos = do 
    c <- MArray.readArray grid pos 
    if elem c "|~" 
        then return () 
        else do 
            mLowPos <- falldown grid pos
            case mLowPos of
                Nothing -> return ()
                Just lowPos -> do 
                    highPos <- fillPool grid lowPos
                    mLeftPos <- branchLeft grid highPos 
                    mRightPos <- branchRight grid highPos 
                    case (mLeftPos, mRightPos) of 
                        (Nothing, Just rightPos) -> fillFromPosition grid rightPos
                        (Just leftPos, Nothing) -> fillFromPosition grid leftPos 
                        (Just leftPos, Just rightPos) -> do 
                            fillFromPosition grid leftPos
                            fillFromPosition grid rightPos

falldown :: STArray s Position Char -> Position -> ST s (Maybe Position)
falldown grid (y, x) = do 
    ymax <- fst <$> snd <$> MArray.getBounds grid
    if y > ymax 
        then return Nothing
        else do 
            val <- MArray.readArray grid (y, x)
            case val of 
                'Q' -> falldown grid (y + 1, x)
                '|' -> return Nothing 
                '~' -> return $ Just (y - 1, x)
                '.' -> do 
                    MArray.writeArray grid (y, x) '|'
                    falldown grid (y + 1, x)
                '#' -> return $ Just (y - 1, x)

fillPool :: STArray s Position Char -> Position -> ST s Position 
fillPool grid pos@(py, px) = do 
    mLeft <- leftWall grid pos 
    mRight <- rightWall grid pos 
    case (mLeft, mRight) of 
        (Just xLeft, Just xRight) -> do 
            mapM_ (\x -> MArray.writeArray grid (py, x) '~') [(xLeft + 1)..(xRight - 1)]
            fillPool grid (py - 1, px)
        _ -> return pos

branchLeft :: STArray s Position Char -> Position -> ST s (Maybe Position)
branchLeft grid pos@(py, px) = do 
    mLeft <- leftWall grid pos
    case mLeft of 
        Just wx -> do 
            mapM_ (\x -> MArray.writeArray grid (py, x) '|') [(wx + 1)..px]
            return Nothing
        Nothing -> do 
            Just freeX <- leftFree grid (py + 1, px)
            mapM_ (\x -> MArray.writeArray grid (py, x) '|') [(freeX)..px]
            return $ Just (py + 1, freeX)

branchRight :: STArray s Position Char -> Position -> ST s (Maybe Position)
branchRight grid pos@(py, px) = do 
    mRight <- rightWall grid pos
    case mRight of 
        Just wx -> do 
            mapM_ (\x -> MArray.writeArray grid (py, x) '|') [px..(wx - 1)]
            return Nothing
        Nothing -> do 
            Just freeX <- rightFree grid (py + 1, px)
            mapM_ (\x -> MArray.writeArray grid (py, x) '|') [px..freeX]
            return $ Just (py + 1, freeX)

leftWall :: STArray s Position Char -> Position -> ST s (Maybe Int)
leftWall grid (y, x) = do 
    xmin <- snd <$> fst <$> MArray.getBounds grid
    if x < xmin
        then return Nothing 
    else do 
        c <- MArray.readArray grid (y, x)
        bc <- MArray.readArray grid (y + 1, x)
        if bc == '.' 
            then return Nothing
            else if c == '#'
                then return $ Just x
                else leftWall grid (y, x - 1)

leftFree :: STArray s Position Char -> Position -> ST s (Maybe Int)
leftFree grid (y, x) = do 
    xmin <- snd <$> fst <$> MArray.getBounds grid
    if x < xmin
        then return Nothing 
    else do 
        c <- MArray.readArray grid (y, x)
        if c == '.'
            then return $ Just x
            else leftFree grid (y, x - 1)

rightWall :: STArray s Position Char -> Position -> ST s (Maybe Int)
rightWall grid (y, x) = do 
    xmax <- snd <$> snd <$> MArray.getBounds grid
    if x > xmax
        then return Nothing 
    else do 
        c <- MArray.readArray grid (y, x)
        bc <- MArray.readArray grid (y + 1, x)
        if bc == '.' 
            then return Nothing
            else if c == '#' 
                then return $ Just x
                else rightWall grid (y, x + 1)

rightFree :: STArray s Position Char -> Position -> ST s (Maybe Int)
rightFree grid (y, x) = do 
    xmax <- snd <$> snd <$> MArray.getBounds grid
    if x > xmax
        then return Nothing 
    else do 
        c <- MArray.readArray grid (y, x)
        if c == '.' 
            then return $ Just x
            else rightFree grid (y, x + 1)

parse :: String -> [Position]
parse = concatMap parseLine . lines 
  where parseLine line = 
          let [(a:'=':d1), (b:'=':ds)] = words line 
              [d2, d3] = map read $ splitOn ".." ds
            in case a of 
                'x' -> [(d, read $ init d1) | d <- [d2..d3]]
                'y' -> [(read $ init d1, d) | d <- [d2..d3]]

makeGrid :: [Position] -> Array Position Char
makeGrid poss = 
    let xmin = pred $ minimum $ map snd poss 
        xmax = succ $ maximum $ map snd poss 
        ymin = pred $ minimum $ map fst poss 
        ymax = maximum $ map fst poss 
          in Array.listArray ((ymin, xmin), (ymax, xmax)) 
               [if (y, x) == (ymin, 500) then 'Q' else if (y, x) `Set.member` (Set.fromList poss) then '#' else '.' 
                 | y <- [ymin..ymax], x <- [xmin..xmax]]

printGrid :: Array Position Char -> String
printGrid grid = unlines [printLine y | y <- [ymin..ymax]]
  where ((ymin, xmin), (ymax, xmax)) = Array.bounds grid
        printLine y = [grid Array.! (y, x) | x <- [xmin..xmax]]