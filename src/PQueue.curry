module PQueue where

data PQueue a = Empty | Node a [PQueue a]

empty :: PQueue a
empty = Empty

singleton :: a -> PQueue a
singleton x = Node x []

isEmpty :: PQueue a -> Bool
isEmpty queue =
    case queue of
        Empty    -> True
        Node _ _ -> False

insertBy :: (a -> a -> Bool) -> a -> PQueue a -> PQueue a
insertBy before x queue = meldBy before (singleton x) queue

minViewBy :: (a -> a -> Bool) -> PQueue a -> Maybe (a, PQueue a)
minViewBy before queue =
    case queue of
        Empty -> Nothing
        Node x children -> Just (x, meldPairsBy before children)

meldBy :: (a -> a -> Bool) -> PQueue a -> PQueue a -> PQueue a
meldBy before queue1 queue2 =
    case queue1 of
        Empty -> queue2
        Node x children1 -> case queue2 of
                Empty -> queue1
                Node y children2 -> if before x y
                                    then Node x (queue2 : children1)
                                    else Node y (queue1 : children2)

meldPairsBy :: (a -> a -> Bool) -> [PQueue a] -> PQueue a
meldPairsBy before queues =
    case queues of
        [] -> Empty
        [queue] -> queue
        queue1 : queue2 : rest -> meldBy before (meldBy before queue1 queue2) (meldPairsBy before rest)