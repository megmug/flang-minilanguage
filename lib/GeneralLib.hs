module GeneralLib where

import Data.List (groupBy, nub, sortBy)

class PrettyPrintable t where
  prettyPrint :: t -> String

-- according to the given relation on type a, calculate an equivalence relation that defines the partitioning
-- this partitioning is then sorted by the reflexive-transitive closure of given relation
topologicallySortedPartitioning :: (Eq a) => [a] -> (a -> a -> Bool) -> [[a]]
topologicallySortedPartitioning xs r = sortBy (partitionOrdering xs r) $ groupBy (correspondingEquivalenceRelation (reflexiveTransitiveClosure xs r)) xs

-- given a list, a relation on that list, create an ordering on the equivalence classes induced by the correspondingEquivalenceRelation of the reflexive-transitive closure of r
partitionOrdering :: (Eq a) => [a] -> (a -> a -> Bool) -> ([a] -> [a] -> Ordering)
partitionOrdering xs r as bs
  | as == bs = EQ
  | or [reflexiveTransitiveClosure xs r a b | a <- as, b <- bs] = LT
  | otherwise = GT

-- calculates the equivalence relation that results from disarding any pairs which are not symmetrically in relation to each other
-- assumes that the relation is transitive to begin with
correspondingEquivalenceRelation :: (Eq a) => (a -> a -> Bool) -> (a -> a -> Bool)
correspondingEquivalenceRelation r a b = a == b || (a `r` b && b `r` a)

-- calculate the reflexive-transitive closure of r on the set xs
reflexiveTransitiveClosure :: (Eq a) => [a] -> (a -> a -> Bool) -> (a -> a -> Bool)
reflexiveTransitiveClosure xs r = tuplesToRelation $ fixedPoint expandedSet (nub $ [(a, a) | a <- xs] ++ relationToTuples xs r)
  where
    expandedSet ts = [(a, b) | a <- xs, b <- xs, x <- xs, (a, b) `elem` ts || ((a, x) `elem` ts && (x, b) `elem` ts)]

fixedPoint :: (Eq a) => (a -> a) -> a -> a
fixedPoint f a = let b = f a in if a == b then b else fixedPoint f b

-- creates a list of tuples from a list and a relation on that list
relationToTuples :: [a] -> (a -> a -> Bool) -> [(a, a)]
relationToTuples xs r = [(a, b) | a <- xs, b <- xs, a `r` b]

-- creates a relation from a list of tuples
tuplesToRelation :: (Eq a) => [(a, a)] -> (a -> a -> Bool)
tuplesToRelation xs a b = (a, b) `elem` xs