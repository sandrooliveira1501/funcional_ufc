module Queue (Queue, 
	         enqueue,dequeue, front, empty, isEmpty) where

data Queue a = Q [a] deriving (Show, Eq)-- representacao por uma lista

enqueue :: a -> Queue a -> Queue a
enqueue x (Q xs) = Q (xs ++ [x])

dequeue :: Queue a -> Queue a 
dequeue ( Q (x:xs) ) = Q xs
dequeue ( Q []     ) = error "Queue.dequeue : empty queue"

front ::Queue a -> a
front ( Q (x:xs) ) = x
front ( Q []     ) = error "Queue.front : empty queue"

empty :: Queue a
empty = Q []

isEmpty :: Queue a -> Bool
isEmpty (Q []) = True
isEmpty (Q _ ) = False

q1 = enqueue 3 empty
q2 = enqueue 4 q1
q3 = enqueue 5 q2 
