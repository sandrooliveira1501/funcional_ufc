module Stack (Stack, push, pop, height, top, empty, isEmpty ) where

data Stack a = Empty | Top a (Stack a)

push :: a -> Stack a -> Stack a
push x p = Top x p

pop :: Stack a -> Maybe (Stack a)
pop Empty     = Nothing
pop (Top x p) = Just p 

height :: Stack a -> Int
height Empty     = 0
height (Top x p) = 1 + height p

top :: Stack a -> Maybe a
top Empty     = Nothing
top (Top x p) = Just x

empty :: Stack a
empty = Empty

isEmpty :: Stack a -> Bool
isEmpty Empty = True
isEmpty p     = False

instance Show a => Show (Stack a) where
	show Empty     = "Pilha de altura " ++ show (height Empty)
	show (Top x p) = "Pilha de altura " ++ show (height (Top x p)) ++ " e topo " ++ show x


