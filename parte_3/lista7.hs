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


data Conj a = Vazio | No a (Conj a) (Conj a) deriving (Show)

vazio :: Conj a
vazio = Vazio

esVazio :: Conj a -> Bool
esVazio Vazio = True
esVazio (No a esq dir) = False

data Complex = Complex {real :: Float, img :: Float}
complex1 = Complex {real = 5, img = 8}
complex2 = Complex {real = 1, img = 2}

add z1 z2 = Complex {real = ((real z1) + (real z2)), img = ((img z1) + (img z2))}
minus z1 z2 = Complex {real = ((real z1) - (real z2)), img = ((img z1) - (img z2))}
multi z1 z2 = Complex {real = (a*a' - b*b' ), img = (a * b' + b * a')}
				where 
                a  = real z1
                a' = real z2
                b  = img z1
                b' = img z2

conjugado z1 = Complex {real = real z1, img = negate (img z1) }
divisao z1 z2 = Complex {real = ((a*c + b*d)/(c*c + d*d)), img = ((b*c - a*d)/(c*c + d*d))}
                where 
                a  = real z1
                c = real z2
                b  = img z1
                d = img z2

negacao z1 = Complex {real = negate (real z1), img = negate (img z1)}

magnitude z = sqrt (x*x + y*y)
        where 
            x = real z
            y = img z 

sinal z = if x == 0 && y == 0 then Complex {real = 0, img = 0} else Complex {real = x/r, img = y/r}
        where 
            x = real z
            y = img z 
            r = magnitude z

instance Num Complex where
	z1 + z2       = add z1 z2
	z1 * z2       = multi z1 z2
	negate z      = negacao z
	abs z         = Complex {real = magnitude z, img=0}
	signum z      = sinal z
	fromInteger z = Complex {real = realToFrac z, img=0}

instance Show Complex where
    show z = show (real z) ++ " + " ++ show (img z) ++ "i"

instance Eq Complex where
    z1 == z2 = ((real z1) == (real z2)) && ((img z1) == (img z2))   

instance Fractional Complex where 
    z1 / z2 = divisao z1 z2
    fromRational a = Complex {real = (fromRational a), img = 0}