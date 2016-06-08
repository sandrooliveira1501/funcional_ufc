-- 1

h :: (Num a, Ord a) => a -> a -> a
h x y = if x >= y then x -y else y-x

-- b)

-- 2 b)

-- 3 a)

-- 4
f :: [Float] -> [Float] -> Float
f xs ys = sum [x * y | (x,y) <- zip xs ys]

-- 5 a)

-- 6 a)

-- 7 c)

-- 8 c)

g [] = []
g (x:_:xs) = x : g xs

-- 9 c)

--10 a)

--11 a)

--12 d)

--13 a)

--14 c)

--15 [4,9,15]

--16
gamma [] = ""
gamma [c] = [c]
gamma (c:u) = c: '.': gamma u

-- incorrect: gamma ['c','.'] ==> "c."

--17
--a)
segundo :: [a] -> a
segundo xs = head (tail xs)

--b)
trocar :: (a,b) -> (b,a)
trocar (x,y) = (y,x)

--c)
par :: a -> b -> (a,b)
par x y = (x,y)

--d)
--dobro :: Num a => a -> a
dobro x = 2*x

--e)
metade :: Fractional a => a -> a
metade x = x/2

--f)
--minuscula :: Char -> Bool
minuscula x = x >= 'a' && x <= 'z'

--g)
intervalo :: Ord a => a -> a -> a-> Bool
intervalo x a b = x >= a && x <= b

--h)
palindromo :: Eq a => [a] -> Bool
palindromo xs = reverse xs == xs

--i)
twice :: (a -> a) -> a -> a
twice f x = f (f x)

--23

ultimo xs = head (reverse xs)

--24
triangulo a b c | a > (b + c) = False
                | b > (c + a) = False
                | c > (a + b) = False
                | otherwise = True

--25
inserir x [] = [x]
inserir x (y:ys) | x <= y = x:(y:ys)
                 | otherwise = y: (inserir x ys)

--26

--a

count1 f [] = 0
count1 f (x:xs) | f x = 1  + (count1 f xs)
                | otherwise = count1 f xs


count2 f xs = length (filter f xs)


-- Lista 2

--1
--a)
interior xs = tail (init xs)

--b)
final x xs = drop ((length xs) - x) xs

--c)
segmento x y xs = take (y-x+1) (drop (x-1) xs)

--d)
extremos n xs = take n xs ++ final n xs

--e)
dotprod xs ys = sum (zipWith (*) xs ys)

--f)
elemento x xs = any (==x) xs

--2)

--a)
somaQuadrados n = sum[x*x | x <- [1 .. n]]

--b)
replica n x = [x | _ <- [1 .. n]]

--c)
linha n = [x + aux | x <- [1 .. n]]
          where aux = sum [1 .. n-1]

--d)
trianguloAritmetico n = [linha x | x <- [1 .. n] ]

--e)
fatores n = [x | x <- [1 .. n], mod n x == 0]

--f)
perfeitos n = (sum (init (fatores n))) == n

--g)
primo x = (fatores x == [1,x])

divide 1 _ = 0
divide a b | mod a b == 0 = 1 + (divide (div a b) b)
           | otherwise = 0

fatoresPrimos x = [(a,divide x a) | a <- [2 .. x], mod x a == 0, primo a]

fatAux 1 _ = []
fatAux n x  | mod n x == 0 = (x, divide n x ):(fatAux (div n (x^(divide n x))) (x+1) )
            | otherwise = fatAux n (x+1)
fatoresPrimos2 n = fatAux n 2

fatoresPrimos3 n = [ (a,b) | a <-[ x| x<-(fatores n), length (fatores x) == 2 ], b<- [maximum[ y | y<-[1..(floor (sqrt ( fromIntegral n) )) ], (mod n  (a^y)) <= 0 ]]  ]
--h)
interseccao xs ys = [x | x <- xs, elem x ys]

--i)
diferenca xs ys = [x | x <- xs, notElem x ys]

--j)
multiplos n = [n*x | x <- [1..n]]
diamonds n = [multiplos x | x <- [1 .. n]] ++ [multiplos x | x <- [n-1, n-2 .. 1]]

--k)
aplica xs x = [f x | f <- xs]

--l)
pitagoricos n = [(a,b,c) | a <- [1 .. n], b <- [1 .. n], c <- [1 .. n], ((a*a) + (b*b)) == (c*c)]

--m)
densa xs = [ (x-1,y) | x <- [n,n-1 .. 1], y <- [(xs !! (n-x))], y /= 0]
        where n = length xs

--n)
neglist xs = sum [1 | x <- xs, x < 0]

--o)
gensquares a b = [x*x | x <- [a .. b], mod x 2 == 0]

--3

--Lista 3

-- 4
--s)

remove x [] = []
remove x (y:ys) | x == y = ys
                | otherwise = y:(remove x ys)

permutations [] = [[]]
permutations xs = [x:y | x <- xs, y <- permutations (remove x xs)]


delete i [] = []
delete i (x:xs) | i == x = xs
                | otherwise = (x:(delete i xs))
permutations2 :: Eq a => [a] -> [[a]]
permutations2 [] = [[]]
permutations2 xs = [ i:j | i <- (xs), j <- permutations2 (delete i xs) ]
