module Queue2 (Queue2,
			  enqueue, dequeue , front, empty, isEmpty ) where

data Queue2 a = Q ( [a], [a] ) deriving (Show, Eq)

norm :: ([a], [a]) -> ([a],[a])
norm ([], tr) = (reverse tr, [])
norm (fr, tr) = (fr, tr)

enqueue :: a -> Queue2 a -> Queue2 a
enqueue x ( Q (fr, tr) ) = Q ( norm (fr, x:tr))

dequeue :: Queue2 a -> Queue2 a
dequeue (Q (x:fr, tr) ) = Q ( norm (fr, tr) )
dequeue _               = error "Queue2.dequeue : empty queue "

front :: Queue2 a -> a
front (Q (x:fr, tr) ) = x
front _               = error "Queue2.front : empty queue"

empty :: Queue2 a
empty = Q ([],[])

isEmpty :: Queue2 a -> Bool
isEmpty (Q ([], _) ) = True
isEmpty (Q (_ , _) ) = False

q1 = enqueue 3 empty
q2 = enqueue 4 q1
q3 = enqueue 5 q2 






