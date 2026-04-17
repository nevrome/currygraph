module PriorityQueue where

data PQ p v
  = Empty
  | Node Int p v (PQ p v) (PQ p v)
-- Int = rank (null-path length)
-- p = priority
-- v = stored value
-- left/right children

rank :: PQ p v -> Int
rank Empty = 0
rank (Node r _ _ _ _) = r

makeNode :: p -> v -> PQ p v -> PQ p v -> PQ p v
makeNode p v a b
  | rank a >= rank b = Node (rank b + 1) p v a b
  | otherwise        = Node (rank a + 1) p v b a

merge :: Ord p => PQ p v -> PQ p v -> PQ p v
merge Empty h = h
merge h Empty = h
merge h1@(Node _ p1 v1 l1 r1) h2@(Node _ p2 v2 l2 r2)
  | p1 <= p2  = makeNode p1 v1 l1 (merge r1 h2)
  | otherwise = makeNode p2 v2 l2 (merge h1 r2)
  
empty :: PQ p a
empty = Empty

-- insert is just merging with a singleton heap.
insert :: Ord p => p -> v -> PQ p v -> PQ p v
insert p v h = merge (Node 1 p v Empty Empty) h

findMin :: PQ p v -> Maybe (p,v)
findMin Empty = Nothing
findMin (Node _ p v _ _) = Just (p,v)

deleteMin :: Ord p => PQ p v -> PQ p v
deleteMin Empty = Empty
deleteMin (Node _ _ _ l r) = merge l r

popMin :: Ord p => PQ p a -> Maybe ((p,a), PQ p a)
popMin Empty = Nothing
popMin (Node _ p v l r) = Just ((p,v), merge l r)
