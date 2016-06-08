-- 2
data Stack a = Empty | Top a (Stack a) deriving (Eq)

--instance Show a => Show (Stack a) where
--        show Empty = "Pilha Vazia"
--        show (Top x p) = "Pilha: " ++ show (array (Top x p))

instance Show a => Show (Stack a) where
        show Empty = "Pilha de altura 0"
        show (Top x p) = "Pilha de altura " ++ (show (height (Top x p))) ++ " e topo " ++ (show x)


toArray Empty = []
toArray (Top x p) = [x] ++ (toArray p)

push :: a -> Stack a -> Stack a
push x Empty = Top x Empty
push x stack = Top x stack

pop :: Stack a -> Maybe (Stack a)
pop Empty = Nothing
pop (Top a p) = Just p

maybeToStack :: Maybe (Stack a) -> Stack a
maybeToStack Nothing = empty
maybeToStack (Just a) = a

height :: Stack a -> Int
height Empty = 0
height (Top x p) = 1 + (height p) 

top :: Stack a -> Maybe a
top Empty = Nothing
top (Top a p) = Just a

empty :: Stack a
empty = Empty

isEmpty :: Stack a -> Bool
isEmpty Empty = True
isEmpty _ = False

p1 = push 2 empty
p2 = push 3 p1
p3 = push 4 p2

--3

--data Queue a = EmptyQ | Start a (Queue a) deriving (Eq,Show) 
data Queue a = EmptyQ | Start a (Queue a) deriving (Eq) 


startQueue :: Queue a -> Maybe a
startQueue EmptyQ = Nothing
startQueue (Start x q ) = Just x

endQueue :: Eq a => Queue a -> Maybe a
endQueue EmptyQ = Nothing
endQueue (Start x q) = if (q == EmptyQ) then (Just x) else (endQueue q)

pushQueue :: a -> Queue a -> Queue a
pushQueue x EmptyQ = Start x EmptyQ
pushQueue x (Start y q) = Start y (pushQueue x q)

popQueue :: Queue a -> Maybe (Queue a)
popQueue EmptyQ = Nothing
popQueue (Start x q) = Just q

emptyQ :: Queue a
emptyQ = EmptyQ

maybeToQueue :: Maybe (Queue a) -> Queue a
maybeToQueue Nothing = EmptyQ
maybeToQueue (Just q) = q

isEmptyQueue :: Queue a -> Bool
isEmptyQueue EmptyQ = True
isEmptyQueue (Start x q) = False

lenQueue :: Queue a -> Int
lenQueue EmptyQ = 0
lenQueue (Start x q) = 1 + (lenQueue q)

whileNotEmpty :: (a -> b) -> (Queue a) -> [b]
whileNotEmpty f EmptyQ = []
whileNotEmpty f (Start x q) = [f x] ++ (whileNotEmpty f q)

instance Show a => Show (Queue a) where
        show EmptyQ = "Fila Vazia de tamanho 0"
        show (Start x q) = "Fila de tamanho " ++ (show (lenQueue (Start x q))) ++ " e início " ++ (show x)

q1 = pushQueue 1 emptyQ
q2 = pushQueue 2 q1


