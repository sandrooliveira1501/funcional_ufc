-- 1 lista 5

scanEsq :: (b -> a -> b) -> b -> [a] -> [b]
scanEsq f n [] = [n]
scanEsq f n (x:xs) = n:(scanEsq f (f n x) xs)

fat x = foldl (*) 1 [2 .. x]

fat2 0 = 1
fat2 x = x  * (fat2 (x-1))

-- 2

fatorialLista = scanEsq (*) 1 [2 ..]

-- 3
fatorial 0 = 1
fatorial n = fatorialLista !! (n-1)

-- 4
--a
parcelas = [ 1 / (fatorial n) | n <- [0,1 .. ]]

--b
e = scanEsq (+) x xs
    where (x:xs) = parcelas

--c

calcE n = sum (take n parcelas)
calcE2 n = e !! (n-1)

--d

absoluteE delta = snd (head (dropWhile (\(a,b) -> (b - a) >= delta ) (zip e (tail e))))

relativeE delta = snd (head (dropWhile (\(a,b) -> ((b-a)/a) >= delta ) (zip e (tail e))))

-- 12

potenciaIterada n f x = last (take (n+1) (iterate f x))

-- 18

intercala x ys = [ (take n ys) ++ [x] ++ (take (length ys - n) (drop n ys)) | n <- [0 .. length ys]]

-- 19

permutacao [] = [[]]
permutacao (x:xs) = concat [(intercala x ys) | ys <- (permutacao xs)]

delete i [] = []
delete i (x:xs) | i == x = xs
               | otherwise = (x:(delete i xs))
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [ i:j | i <- (xs), j <- permutations (delete i xs) ]

-- ap 2

--1

--a

data Arv a = Vazia | No a (Arv a) (Arv a) deriving (Show)

a0 = No 5 Vazia Vazia
a1 = No 1 Vazia Vazia
a2 = No 2 a1 Vazia
a3 = No 3 a2 a0

a4 = No 9 Vazia Vazia
a5 = No 28 Vazia Vazia
a6 = No 50 Vazia Vazia
a7 = No 18 Vazia Vazia
a8 = No 29 a5 Vazia
a9 = No 40 Vazia a6
a10 = No 30 a8 a9
a11 = No 19 a7 Vazia
a12 = No 20 a11 a10
a13 = No 10 a4 a12

nivel _ Vazia = []
nivel 0 (No x esq dir) = [x]
nivel n (No x esq dir) = (nivel (n-1) esq) ++ (nivel (n-1) dir)

inserir y Vazia = No y Vazia Vazia
inserir y (No x esq dir) | (y == x) = (No x esq dir)
                         | (y > x) = (No x esq (inserir y dir))
                         | otherwise = (No x (inserir y esq) (dir))

listar :: Ord a => Arv a -> [a]
listar Vazia = []
listar (No x esq dir) = (listar dir) ++ [x] ++ (listar esq)

mais_esq :: Arv a -> a
mais_esq (No x Vazia _) = x
mais_esq (No _ esq _) = mais_esq esq

remover x Vazia = Vazia
remover x (No y Vazia dir) | x == y = dir
                           | otherwise = (No y Vazia $ remover x dir)
remover x (No y esq Vazia) | x == y = esq
                           | otherwise = (No y Vazia $ remover x esq)
remover x (No y esq dir) | x == y = No (mais_esq dir) esq (remover (mais_esq dir) dir)
                         | x > y = No y esq (remover x dir)
                         | otherwise = No y (remover x esq) dir


--2

data Prop = Var Char
          | Neg Prop
          | Conj Prop Prop
          | Disj Prop Prop

p1 = (Conj (Var 'a') (Disj (Var 'b') (Var 'a')))

propToArray (Var a) = [a]
propToArray (Neg p) = propToArray p
propToArray (Conj p q) = propToArray p ++ propToArray q
propToArray (Disj p q) = propToArray p ++ propToArray q

deleteVar x ys = [y | y <- ys, y /= x]

contarAux [] = []
contarAux (x:xs) = [(x, lx - ly)] ++ contarAux ys
              where ys = deleteVar x xs
                    ly = length ys
                    lx = length (x:xs)

contar p = contarAux (propToArray p)

-- lista 6

type Vert = Int
type Grafo = ([Vert], [(Vert, Vert)])

g = ([1 ,2 ,3] , [(1 , 2) , (2 , 1) , (2 , 3)])

existeAresta (vs, []) (a,b) = False
existeAresta (vs, ((x,y):ys)) (a,b) = if (x,y) == (a,b) then True else existeAresta (vs, ys) (a,b)   

caminhoAux g [] = True
caminhoAux g [x] = True
caminhoAux g (x:y:xs) = if (existeAresta g (x,y)) then (caminhoAux g (y:xs)) else False

caminho g xs = if (length xs) <= 1 then error "caminho inválido" else caminhoAux g xs

-- ap 2

type Rel = [(Int, Int)]

hasRel (a,b) [] = False
hasRel (a,b) ((x,y):xs) = if (a,b) == (x,y) then True else hasRel (a,b) xs

removeRel (a,b) [] = []
removeRel (a,b) ((x,y):xs) = if (a,b) == (x,y) then xs else (x,y):(removeRel (a,b) xs)


-- a
simetrica [] = True
simetrica ((x,y):xs) = if hasRel (y,x) ((x,y):xs) then (simetrica (removeRel (y,x) xs)) else False  

-- b

iniciaCom a [] = []
iniciaCom a ((x,y):xs) = if a == x then (x,y):(iniciaCom a xs) else (iniciaCom a xs) 

terminaCom a [] = []
terminaCom a ((x,y):xs) = if a == y then (x,y):(terminaCom a xs) else (terminaCom a xs) 

transitivaAux 0 _ = True
transitivaAux n xs = (and [hasRel (a,y) xs | (x,y) <- i]) && transitivaAux (n-1) xs 
                     where
                            (a,b) = xs !! (length xs - n)
                            i = iniciaCom b xs

transitiva xs = transitivaAux (length xs) xs


-- Lista 4 

mystery xs = foldl (++) [] (map sing xs)
sing x = [x]

-- 1
media [] = 0
media xs = (realToFrac (sum xs)) / (realToFrac (length xs))

divideMedia xs = ((filter (<= m) xs), (filter (>= m) xs))
            where m = media xs

-- 2

fatAcc 0 = []
fatAcc n = scanl (*) 1 [2..n]

fatorial2 n = last (fatAcc n)


-- 7

relacionados p [] = True
relacionados p [x] = True
relacionados p (x:y:xs) = if p x y then relacionados p (y:xs) else False

relacionados2 p xs = and [ p a b | (a,b) <- (zip xs (tail xs))]

-- 21 

inserir2 y [] = [y]
inserir2 y (x:xs) | y <= x = (y:x:xs)
                 | otherwise = x:(inserir2 y xs) 


-- adicionais

scanl2 f y [] = [y]
scanl2 f y (x:xs) = y:(scanl2 f (f y x) xs)


safeLog n | n > 0 = Just (log n)
          | otherwise = Nothing 