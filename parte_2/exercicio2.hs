
-- 1,2,3,4,7,10,15

-- Questão 1

--scan func list = reverse $ foldl (\l e -> (func e (head l)) : l) [head list] (tail list)
--scan f y ys = foldl (\a x -> (f (head a) x):a) [y] ys

-- Parâmetros invertidos em relação ao exemplo
scanEsq f a [] = [a]
scanEsq f a (x:xs) = a:(scanEsq f (f a x) xs)

-- 2

fatorialLista = scanEsq (*) 1 [2 ..]

-- 3

fatorial n | n > 0 = fatorialLista !! (n-1)
           | n == 0 = 1
           | otherwise = error ("Permitido apenas valores >= 0")

-- 4

--a)

parcelas = [ 1 / (fatorial x) | x <- [0 ..]]

--b)

e = scanEsq (+) (head parcelas) (tail parcelas)

--c)

calcE 0 = 0
calcE n = e !! (n-1)

-- 10  = 2.7182815255731922
-- 100 = 2.7182818284590455
-- 1000 = 2.7182818284590455

--d)

absolutoE2 n = head [y | (x,y) <- zip e (tail e), abs (y-x) < n]

absolutoE n = snd (head (dropWhile (\(x,y) -> (y-x) >= n) (zip e (tail e))))

relativoE n = snd (head (dropWhile (\(x,y) -> ((y-x)/x) >= n) (zip e (tail e))))

--7
--geraConjunto 0 xs = [[]]
--geraConjunto x xs = [ a:b | a <- xs,b <- (geraConjunto (x-1) xs)]
--kleene xs = concat [geraConjunto x xs | x <- [0 ..]]

kleene xs = []:[ y ++ [x] | y <- (kleene xs), x <- xs]

--10

--eco xs = ecoAux 1 xs
--ecoAux n [] = []
--ecoAux n (x:xs) = [ x | _ <- [1 .. n]] ++ (ecoAux (n+1) xs)

eco xs = [x | (x,y) <- (zip xs [0 ..]), _ <- [0 .. y] ]

--15
digitos n = [read [x] :: Int | x <- show n]

--a)

--listaInteger xs = sum[ (xs !! x) * (10 ^ (s-x)) | x <- [s,s-1 .. 0] ]
--                  where s = length xs - 1

listaInteger [] = error ("O parâmetro deve ser uma lista não vazia e conter apenas inteiros")
listaInteger xs = read (concat [ show x | x <- xs]) :: Int

--b)

juntaNumero x y = listaInteger (digitos x ++ digitos y)

--c)

inverso n = listaInteger (reverse (digitos n))
