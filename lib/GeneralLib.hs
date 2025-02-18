module GeneralLib where

import Data.List (groupBy, nub, sortBy)

class PrettyPrintable t where
  prettyPrint :: t -> String

-- given a starting value, calculates the least fixed point of f given a, which is the value f^n a such that (f^n a) = (f^(n + 1) a) for minimal n
fixedPoint :: (Eq a) => (a -> a) -> a -> a
fixedPoint f a = let b = f a in if a == b then b else fixedPoint f b

-- according to the given relation on type a, calculate an equivalence relation that defines the partitioning
-- this partitioning is then sorted by the reflexive-transitive closure of given relation
topologicallySortedPartitioning :: (Eq a) => [a] -> (a -> a -> Bool) -> [[a]]
topologicallySortedPartitioning xs r = sortBy (partitionOrdering xs r) $ groupBy (symmetricFilter (reflexiveTransitiveClosure xs r)) xs

-- perform a topological sort given a (dependency) relation
topologicallySort :: (Eq a) => [a] -> (a -> a -> Bool) -> Maybe [a]
topologicallySort [] _ = Just []
topologicallySort (x : xs) r
  | transitiveClosure (x : xs) r x x = Nothing
  | not $ any (r x) xs = do
      sortedDefs <- topologicallySort xs r
      return $ x : sortedDefs
  | otherwise = topologicallySort (xs ++ [x]) r

-- given a list, a relation on that list, create an ordering on the equivalence classes induced by the correspondingEquivalenceRelation of the reflexive-transitive closure of r
partitionOrdering :: (Eq a) => [a] -> (a -> a -> Bool) -> ([a] -> [a] -> Ordering)
partitionOrdering xs r as bs
  | as == bs = EQ
  | or [reflexiveTransitiveClosure xs r a b | a <- as, b <- bs] = LT
  | otherwise = GT

-- filters r to only those pairs it is symmetric on
-- consequently, is the identiy for any symmetric relation
-- if the input relation is reflexive and transitive, the result is an equivalence relation
symmetricFilter :: (a -> a -> Bool) -> (a -> a -> Bool)
symmetricFilter r a b = a `r` b && b `r` a

-- calculate the reflexive-transitive closure of r on the set xs
reflexiveTransitiveClosure :: (Eq a) => [a] -> (a -> a -> Bool) -> (a -> a -> Bool)
reflexiveTransitiveClosure xs r = tuplesToRelation $ setTransitiveClosure (relationToTuples xs $ reflexiveClosure r)

transitiveClosure :: (Eq a) => [a] -> (a -> a -> Bool) -> a -> a -> Bool
transitiveClosure xs r = tuplesToRelation $ setTransitiveClosure $ relationToTuples xs r

reflexiveClosure :: (Eq a) => (a -> a -> Bool) -> (a -> a -> Bool)
reflexiveClosure r a b = a == b || a `r` b

setTransitiveClosure :: (Eq a) => [(a, a)] -> [(a, a)]
setTransitiveClosure ts = fixedPoint expandedSet ts
  where
    baseSet = flatten ts
    expandedSet ts' = nub $ [(a, b) | a <- baseSet, b <- baseSet, x <- baseSet, (a, b) `elem` ts' || ((a, x) `elem` ts' && (x, b) `elem` ts')]

flatten :: (Eq a) => [(a, a)] -> [a]
flatten ts = nub $ concat [[a, b] | (a, b) <- ts]

-- creates a list of tuples from a list and a relation on that list
relationToTuples :: [a] -> (a -> a -> Bool) -> [(a, a)]
relationToTuples xs r = [(a, b) | a <- xs, b <- xs, a `r` b]

-- creates a relation from a list of tuples
tuplesToRelation :: (Eq a) => [(a, a)] -> (a -> a -> Bool)
tuplesToRelation xs a b = (a, b) `elem` xs