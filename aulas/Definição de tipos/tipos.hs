data Figura = Circ Float | Rect Float Float deriving (Eq, Show)

area :: Figura -> Float
area (Circ r)   = pi*r^2 
area (Rect w h) = w*h


safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

safediv :: Int->Int->Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (div n m)


data Expr = Val Int
		  | Soma Expr Expr
		  | Mult Expr Expr deriving (Show)

expr1 = Mult (Soma (Val 4) (Val 5)) (Val 6)

valor:: Expr -> Int
valor (Val n)      = n
valor (Soma e1 e2) = valor e1 + valor e2
valor (Mult e1 e2) = valor e1 * valor e2 


data Arv a = Vazia | No a (Arv a) (Arv a) deriving (Show)

arv1 = foldr (insere) Vazia [2,3,4,5]

tamanhoArv :: Arv a -> Int
tamanhoArv Vazia = 0
tamanhoArv (No x esq dir) = 1 + tamanhoArv esq + tamanhoArv dir

ocorre :: Ord a => a -> Arv a -> Bool
ocorre y Vazia = False
ocorre y (No x esq dir) | x == y = True
						| y <  x = ocorre y esq
						| y >  x = ocorre y dir 

insere :: Ord a => a -> Arv a -> Arv a
insere x Vazia          = No x Vazia Vazia
insere x (No y esq dir) | x <= y    = No y (insere x esq) dir
						| otherwise = No y esq (insere x dir)
