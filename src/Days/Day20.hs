module Days.Day20(run) where

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Set as Set 
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Array as Array
import Data.Array (Array)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)

data Regex = Segment [Char] | Union [Regex] | Concat Regex Regex
  deriving (Show)

type Position = (Int, Int)
type Edge = (Position, Position)

run :: String -> String
run s = 
  let r = evalState parse s 
    in show (fst $ solve r) ++ ", " ++ show (snd $ solve r)

parse :: State String Regex
parse = do 
  rest <- get
  case rest of 
    '(':xs -> do 
      modify tail
      union <- parseUnion
      remainder <- get
      case remainder of 
        '$':[] -> return union
        ')':_ -> return union
        _ -> do
          afterUnion <- parse
          return $ Concat union afterUnion
    '$':[] -> do 
      modify tail
      return $ Segment []
    '^':_ -> do 
      modify tail
      parse
    _ -> do
      segment <- parseSegment
      remainder <- get
      case remainder of 
        '|':_ -> return segment
        ')':_ -> return segment
        '$':[] -> do 
          modify tail
          return segment
        _ -> do
          afterSegment <- parse
          return $ Concat segment afterSegment

parseSegment :: State String Regex
parseSegment = do 
  (sequence, remainder) <- span (\c -> not $ elem c "|$()") <$> get 
  put remainder
  return $ Segment sequence

parseUnion :: State String Regex 
parseUnion = do
  firstRegex <- parse
  remainder <- get
  case remainder of
    '|':xs -> do 
      modify tail
      Union regexes <- parseUnion
      return $ (Union $ firstRegex : regexes)
    ')':xs -> do 
      modify tail
      return $ Union [firstRegex]

visit :: Set Position -> Regex -> Writer (Set Edge) (Set Position)
visit ps (Segment []) = return ps
visit ps (Segment (c:cs)) = do
  tell $ Set.fromList [(p, walk c p) | p <- Set.elems ps]
  visit (Set.map (walk c) ps) (Segment cs)
visit ps (Concat r1 r2) = do 
  qs <- visit ps r1
  visit qs r2
visit ps (Union rs) = do 
  mconcat <$> mapM (visit ps) rs

solve :: Regex -> (Int, Int)
solve r = 
  let edges = symmetricClosure $ execWriter (visit (Set.singleton (0, 0)) r)
      adjList = makeAdjList edges
      distances = bfs adjList (Map.singleton (0, 0) 0) (Seq.singleton (0, 0))
    in (maximum $ Map.elems distances, length $ filter (>= 1000) $ Map.elems distances)

walk :: Char -> Position -> Position
walk 'N' (y, x) = (y - 1, x)
walk 'E' (y, x) = (y, x + 1)
walk 'S' (y, x) = (y + 1, x)
walk 'W' (y, x) = (y, x - 1)

symmetricClosure :: (Ord a) => Set (a, a) -> Set (a, a)
symmetricClosure as = Set.union as (Set.map (\(a, b) -> (b, a)) as) 

makeAdjList :: Set Edge -> Map Position [Position]
makeAdjList es = Map.fromListWith (++) [(p1, [p2]) | (p1, p2) <- Set.elems es]

bfs :: Map Position [Position] -> Map Position Int -> Seq Position -> Map Position Int
bfs adjList dists Seq.Empty = dists
bfs adjList dists (n Seq.:<| ns) = 
  let currentDist = dists Map.! n
      newSuccs = filter (\p -> Map.lookup p dists == Nothing) $ adjList Map.! n
      updatedDists = Map.union dists (Map.fromList [(p, succ currentDist) | p <- newSuccs])
    in bfs adjList updatedDists (ns Seq.>< (Seq.fromList newSuccs))