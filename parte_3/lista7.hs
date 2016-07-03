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

-- 10

data Conj a = Vazio | No a (Conj a) (Conj a) deriving (Show, Eq, Ord)

conj1 = No 1 Vazio Vazio
conj2 = No 2 conj1 Vazio
conj3 = No 10 Vazio Vazio
conj4 = No 5 conj2 conj3

conj21 = No 3 Vazio Vazio
conj22 = No 4 conj21 Vazio
conj23 = No 11 Vazio Vazio
conj24 = No 5 conj22 conj23


vazio :: Conj a
vazio = Vazio

esVazio :: Conj a -> Bool
esVazio Vazio = True
esVazio (No a esq dir) = False

pertence :: Ord a => a -> Conj a -> Bool
pertence x Vazio = False
pertence x (No a esq dir) | x == a = True
                          | x < a = pertence x esq
                          | otherwise = pertence x dir

insere :: Ord a => a -> Conj a -> Conj a
insere x Vazio = No x Vazio Vazio
insere x (No a esq dir) | x == a = (No a esq dir)
                        | x < a = No a (insere x esq) dir
                        | otherwise = No a esq (insere x dir)

mais_esq :: Conj a -> a
mais_esq (No x Vazio _) = x
mais_esq (No _ esq _) = mais_esq esq

elimina :: Ord a => a -> Conj a -> Conj a
elimina x Vazio = Vazio
elimina x (No y Vazio dir) = if x == y then dir else (No y Vazio (elimina x dir))
elimina x (No y esq Vazio) = if x == y then esq else (No y (elimina x esq) Vazio)

elimina x (No y esq dir) 
                        | x > y = (No y esq (elimina x dir))
                        | x < y = (No y (elimina x esq) dir)
                        | otherwise = No (mais_esq dir) esq (elimina (mais_esq dir) dir)

subconjunto :: Ord a => Conj a -> Conj a -> Bool
subconjunto Vazio _ = True
subconjunto _ Vazio = False
subconjunto (No a esq dir) q = if pertence a q then (subconjunto esq q) && (subconjunto dir q) else False 

subconjuntoProprio :: Ord a => Conj a -> Conj a -> Bool
subconjuntoProprio Vazio Vazio = False
subconjuntoProprio p q = (subconjunto p q) && (not (subconjunto q p))

cardinal :: Conj a -> Int
cardinal Vazio = 0
cardinal (No a esq dir) = 1 + cardinal esq + cardinal dir

uniao :: Ord a => Conj a -> Conj a -> Conj a
uniao Vazio q = q
uniao (No a esq dir) q = (uniao dir (uniao esq (insere a q)))

uniaoLista :: Ord a => [Conj a] -> Conj a
uniaoLista [] = Vazio
uniaoLista [x] = x
uniaoLista (x:y:xs) = uniaoLista ((uniao x y):xs)

interseccao :: Ord a => Conj a -> Conj a -> Conj a
interseccao Vazio _ = Vazio
interseccao p Vazio = Vazio
interseccao (No a esq dir) q = if pertence a q then (No a (interseccao esq q) (interseccao dir q)) else interseccao (elimina a (No a esq dir)) q 

disjuntos :: Ord a => Conj a -> Conj a -> Bool
disjuntos p q = if esVazio (interseccao p q) then True else False

diferenca :: Ord a => Conj a -> Conj a -> Conj a
diferenca Vazio _ = Vazio
diferenca p Vazio = p
diferenca (No a esq dir) q = if not (pertence a q) then (No a (diferenca esq q) (diferenca dir q)) else diferenca (elimina a (No a esq dir)) q 

filtraConj :: Ord a => (a -> Bool) -> Conj a -> Conj a    
filtraConj p Vazio = Vazio
filtraConj p (No a esq dir) | p a = No a (filtraConj p esq) (filtraConj p dir)
                            | otherwise = filtraConj p (elimina a (No a esq dir)) 

mapConj :: (a -> b) -> Conj a -> Conj b
mapConj _ Vazio = Vazio
mapConj f (No a esq dir) = No (f a ) (mapConj f esq) (mapConj f dir)

-- potencia

toList Vazio = []
toList (No a esq dir) = (toList esq) ++ [a] ++ (toList dir)

fromList [] = Vazio
fromList (x:xs) = insere x (fromList xs)

subsets :: [a] -> [[a]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

potencia conj = fromList [fromList x | x <- perm ]
                where perm = subsets (toList conj)

-- 11

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