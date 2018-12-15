{-# LANGUAGE TemplateHaskell #-}

module Days.Day3(run) where

import Control.Lens 
import Text.Parsec
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)

data Position = Position {
    _x :: Int,
    _y :: Int
} deriving (Show, Eq, Ord)
makeLenses ''Position

data Rectangle = Rectangle {
    _topLeft :: Position,
    _bottomRight :: Position
} deriving (Show)
makeLenses ''Rectangle

run :: String -> String
run s = let claims = parseClaims s 
        in show (coveredTwice . coverCounts $ claims) ++ ", " ++ show (singleClaim claims)

-- parse input
parseClaims :: String -> [Rectangle]
parseClaims s = case parse parser "" s of 
    Left err -> error (show err)
    Right rects -> rects

parser :: Parsec String st [Rectangle]
parser = do 
    rects <- many parseLine
    eof
    return rects

parseLine :: Parsec String st Rectangle
parseLine = do 
    char '#'
    number
    string " @ "
    px <- number
    char ','
    py <- number
    string ": "
    w <- number
    char 'x'
    h <- number
    endOfLine
    return Rectangle { 
        _topLeft=Position {_x=px, _y=py}, 
        _bottomRight=Position {_x=px + w - 1, _y=py + h - 1}
    }

number :: Parsec String st Int
number = read <$> many1 digit

-- get grid points covered at least once and how often they are covered
coverCounts :: [Rectangle] -> Map Position Int
coverCounts = foldr addRect Map.empty

addRect :: Rectangle -> Map Position Int -> Map Position Int
addRect rect counts = foldr bumpPos counts (positions rect)

positions :: Rectangle -> [Position]
positions rect = [Position {_x=px, _y=py} |
    px <- [rect ^. topLeft . x..rect ^. bottomRight . x], 
    py <- [rect ^. topLeft . y..rect ^. bottomRight . y]]

bumpPos :: Position -> Map Position Int -> Map Position Int
bumpPos pos = Map.insertWith (+) pos 1

coveredTwice :: Map Position Int -> Int
coveredTwice = length . filter (>1) . Map.elems

-- get index of claim with no overlap
overlap :: Rectangle -> Rectangle -> Int
overlap rect1 rect2 = let 
    leftRight = minimum $ map (view (bottomRight . x)) [rect1, rect2]
    rightLeft = maximum $ map (view (topLeft . x)) [rect1, rect2]
    lowerUp   = maximum $ map (view (topLeft . y)) [rect1, rect2]
    upperDown = minimum $ map (view (bottomRight . y)) [rect1, rect2]
    in (length [rightLeft..leftRight]) * (length [lowerUp..upperDown])

singleClaim :: [Rectangle] -> Int
singleClaim claims = head [i | (i, claim) <- numbered claims, 
                           all (\(j, other) -> i == j || overlap claim other == 0) (numbered claims)
                          ]
    where numbered = zip [1..]