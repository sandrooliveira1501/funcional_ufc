data Arv a = Vazia | No a (Arv a) (Arv a) deriving (Show)

a1 = No 1 Vazia Vazia
a2 = No 2 a1 Vazia
a3 = No 10 Vazia Vazia
a4 = No 5 a2 a3

nivel :: Int -> Arv a -> [a]
nivel _ Vazia = []
nivel 0 (No x esq dir) = [x]
nivel n (No x esq dir) = (nivel (n-1) esq) ++ (nivel (n-1) dir) 

inserir :: Ord a => a -> Arv a -> Arv a
inserir x Vazia = (No x Vazia Vazia)
inserir x (No y esq dir)  
						| x == y = (No y esq dir)
						| x > y = (No y esq (inserir x dir)) 
						| otherwise = (No y (inserir x esq) dir)


listar :: Ord a => Arv a -> [a]
listar Vazia = []
listar (No x esq dir) = (listar esq) ++ [x] ++ (listar dir)

mais_esq :: Arv a -> a
mais_esq (No x Vazia _) = x
mais_esq (No _ esq _) = mais_esq esq

remover :: Ord a => a -> Arv a -> Arv a
remover x Vazia = Vazia
remover x (No y Vazia dir) = if x == y then dir else (No y Vazia (remover x dir))
remover x (No y esq Vazia) = if x == y then esq else (No y (remover x esq) Vazia)

remover x (No y esq dir) 
 						| x > y = (No y esq (remover x dir))
						| x < y = (No y (remover x esq) dir)
						| otherwise = No (mais_esq dir) esq (remover (mais_esq dir) dir)


lista_no Vazia = []
lista_no (No x Vazia Vazia) = [x]
lista_no (No x esq dir) = (lista_no esq) ++ (lista_no dir)

predicado_no f Vazia = True
predicado_no f (No x Vazia Vazia) = f x
predicado_no f (No x esq dir) = (predicado_no f esq) && (predicado_no f dir)

got (a,b) xs = not (null (foldr (\(a',b') ys -> if (a,b) == (a',b') then (a',b'):ys else ys ) [] xs))

got2 (a,b) xs = not $ null $ foldl (\ys (a',b') -> if (a,b) == (a',b') then (a',b'):ys else ys) [] xs

concatena xs = foldr (\y ys-> y ++ ys) [] xs

inverte xs = foldr (\y ys -> ys ++ [y]) [] xs

tamanho xs = foldr (\y a -> 1 + a) 0 xs

paridade xs = foldr (\y a -> if y then y && (not a) else a) False xs

paridade2 xs = foldl (\a y -> if y then y && (not a) else a) False xs

prefixos [] = []
prefixos xs = (prefixos (init xs)) ++ [xs]

scanSum xs = [ sum ys    | ys <- (prefixos xs)]


subInicial [] = []
subInicial (x:xs) = x:(takeWhile (\y -> x == y) xs)

dropSubInicial [] = []
dropSubInicial (x:xs) = dropWhile (\y -> x == y) xs

group [] = []
group (x:xs) = (x,(length (subInicial (x:xs)))):(group (dropSubInicial (x:xs)))

agrupa [] = []
agrupa xss  | menorLista == 0 = []
            | otherwise = (map (\xs -> head xs) xss):(agrupa (map (\xs -> tail xs) xss))
                where menorLista = minimum (map length xss)


insert a [] = [a]
insert a (x:xs) | a <= x = a:x:xs
                | otherwise = x:(insert a xs)

insertionSort [] = []
insertionSort [x] = [x]
insertionSort (x:xs) = insert x (insertionSort xs)


data MConj a = Vazio | No2 a Int (MConj a) (MConj a)

mconj1 = No2 'A' 2 Vazio (No2 'B' 1 Vazio Vazio)
mconj2 = No2 1 2 Vazio (No2 2 1 Vazio Vazio)

listarM Vazio = [] 
listarM (No2 a n esq dir) = (listarM esq) ++ [a | aux <- [1 .. n]] ++ (listarM dir)

sumMConj Vazio = 0 
sumMConj (No2 a n esq dir) = (sumMConj esq) + ((toInteger a) * (toInteger n)) + (sumMConj dir)
