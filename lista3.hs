-- Questão 1

--a)

  --type
  remove :: Eq a => a-> [a] -> [a]
  remove _ [] = []
  remove x (y:ys) = if x == y then ys else y: (remove x ys)

--b)

  --type
  partes :: [a] -> [[a]]
  partes [] = [ [] ]
  partes (x:xs) = [x:y | y <- partes xs] ++ partes xs  

--c)
  rota :: Int -> [a] -> [a]
  rota n xs = drop n xs ++ take n xs

--d)
  swap :: (a,b) -> (b,a)
  swap (x,y) = (y,x)

--e)
  twice :: (a -> a) -> a -> a
  twice f x = f (f x)

-- Questão 2

{--
a) A função remove todas as ocorrências do parâmetro x de uma lista
b) A função retorna todos os subconjuntos formado pelos elementos da lista passada por parâmetro
c) A função rotaciona a lista, colocando os n primeiros elementos como os n últimos elementos da lista retornada
d) A função recebe uma tupla (x,y) e retorna uma tupla com os elementos trocados (y,x)
e) A função recebe uma função f e um parâmetro x, então a função f é aplicada duas vezes ao parâmetro x
--}

-- Questão 3
--a)
  prodIntervalo :: Int -> Int -> Int
  prodIntervalo x y = product [x .. y]

--b)
  somaIntervalo :: Int -> Int -> Int
  somaIntervalo x y = sum [x .. y] 

--c)
  metades :: [a] -> ([a],[a])
  metades xs = splitAt (div (length xs) 2) xs 

-- Questão 4

--a)

  potencia _ 0 = 1
  potencia x 1 = x
  potencia x y = x * (potencia x (y-1))

--b) 
  
  elemento x [] = False
  elemento x (y:ys) | x == y = True
                    | otherwise = elemento x ys 

--c)
  seleciona _ [] = error("Posição")
  seleciona x (y:ys)  | x == 0 = y
                      | otherwise = seleciona (x-1) ys 

--d)
  refinada [] = []
  refinada [x] = [x]
  refinada (x:y:ys) = [x,(x+y)/2] ++ refinada (y:ys)

--e)
  merge [] ys = ys
  merge xs [] = xs
  merge (x:xs) (y:ys) | x <= y = x:(merge xs (y:ys)) 
                      | otherwise = y:(merge (x:xs) ys)

--f)
  mergeSort [] = []
  mergeSort [x] = [x]
  mergeSort xs = merge (mergeSort as) (mergeSort bs)  
              where (as,bs) = metades xs

--g)
  ordenada [] = True
  ordenada [x] = True
  ordenada (x:y:ys) | x <= y = ordenada (y:ys)
                    | otherwise = False

--h)

  subconjunto [] _ = True
  subconjunto (x:xs) ys | elem x ys = subconjunto xs ys
                        | otherwise = False

--i)

  union [] ys = ys
  union xs [] = xs
  union (x:xs) ys | notElem x ys = x:(union xs ys)
                  | otherwise = union xs ys

--j)

  diferencia xs []  = xs
  diferencia [] _   = []
  diferencia (x:xs) ys  | notElem x ys = x:(diferencia xs ys)
                        | otherwise = diferencia xs ys

--k)
  frequencia x [] = 0
  frequencia x (y:ys) | x == y = 1 + (frequencia x ys)
                      | otherwise = frequencia x ys


--l)

  unico x [] = False
  unico x (y:ys)  | x == y = notElem x ys
                  | otherwise = unico x ys

--m)

  maiorSalto [] = 0
  maiorSalto [x] = 0
  maiorSalto (x:y:ys) = if dif > prox then dif else prox
                    where dif = abs(x - y); prox = maiorSalto (y:ys)


--n)

  horner [] z = 0
  horner (c:cs) z = c + z * (horner cs z)  

--o)
--i)
  fac x = fac' 1 x
            where fac' n 1 = n
                  fac' n x = fac' (n*x) (x-1)  

--ii)
  reverse2 xs = reverse' [] xs
            where reverse' ys [] = ys
                  reverse' ys (x:xs) = reverse' ([x] ++ ys) xs

--p)
  remove2 _ [] = []
  remove2 x (y:ys) | x == y = remove2 x ys
                  | otherwise = (y:(remove2 x ys))

--q)
  unique [] = []
  unique (x:xs) = x: unique (remove x xs)

--r)
  inserir x [] = [x]
  inserir x (y:ys) | x < y = x:y:ys
                    | otherwise = y: (inserir x ys)

--s)
  delete i [] = []
  delete i (x:xs) | i == x = xs
                  | otherwise = (x:(delete i xs))
  permutations :: Eq a => [a] -> [[a]]
  permutations [] = [[]]
  permutations xs = [ i:j | i <- (xs), j <- permutations (delete i xs) ]