module Utility.UnionFind(Edge, unionFind, groupOf, regions) where
  
import Control.Monad.ST
import Data.Array.ST
import Control.Monad
import Data.List hiding (find)

-- type for edges over some index type
type Edge a = (a, a)
  
-- apply union find algorithm for each edge
unionFind :: (Ix a) => [Edge a] -> STArray s a a -> ST s ()
unionFind [] _ = return ()
unionFind ((a,b):es) arr = do
  ra <- find arr a
  rb <- find arr b
  if ra == rb 
    then unionFind es arr
    else do
      writeArray arr ra rb
      unionFind es arr
    
-- find root of union find set
find :: (Ix a) => STArray s a a -> a -> ST s a
find arr a = do
  pa <- readArray arr a
  if pa == a 
    then return a
    else find arr pa
    
-- return group of one given index
groupOf :: (Ix a) => STArray s a a -> a -> ST s [a]
groupOf arr i = do 
  is <- getIndices arr
  filterM (\a -> (==) <$> find arr i <*> find arr a) is
    
-- return number of regions
regions :: (Ix a) => STArray s a a -> ST s Int
regions arr = do
  is <- getIndices arr
  roots <- mapM (\a -> find arr a) is
  return $ length . nub $ roots
  
-- get indices from an array
getIndices :: (Ix a) => STArray s a a -> ST s [a]
getIndices arr = map fst <$> getAssocs arr

          