module Stack (Stack, --exportar o tipo 
			  push, pop, top, empty, isEmpty ) where --exportar as operações

data Stack a = Stk [a] deriving (Show, Eq) -- implementação usando listas	

push :: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x:xs)

pop :: Stack a -> Stack a
pop ( Stk (_:xs) ) = Stk xs
pop ( Stk []     ) = error "Stack.pop: empty stack"

top :: Stack a -> a
top (Stk (x:_) ) = x
top (Stk []    ) = error "Stack.top : empty stack"

empty :: Stack a
empty = Stk []

isEmpty :: Stack a -> Bool
isEmpty (Stk [] ) = True
isEmpty (Stk _  ) = False 

p1 = push 2 empty
p2 = push 4 p1
p3 = push 5 p2