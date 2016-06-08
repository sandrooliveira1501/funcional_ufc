somaQuadrado n = sum [x * x | x <- [1 .. n]]

replica n x = [ x | y <- [1 .. n]]

linha n = [x | x <- [(s + 1) .. (s + n)]]
          where s = sum[1 .. n-1]

triangulo n = [linha x | x <- [1 .. n]]

-- Lista - Entrega

aplica :: [a -> a] -> a -> [a]
aplica xs y = [x (y) | x <- xs]

pitagoricos :: Int -> [(Int, Int, Int)]
pitagoricos n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

densa :: [Int] -> [(Int,Int)]
densa xs = [(a,b) | (a,b) <- (zip (reverse [0 .. ((length xs) - 1)]) xs), b /= 0]

neglist :: Num a => Num b => Ord b => [b] -> a
neglist xs = sum [1 | x <- xs, x < 0]

neglist2 xs = length [x | x <- xs, x < 0]

-- Lista Completa

-- 1-a

interior :: [a] -> [a]
interior xs =  tail (init xs)

final n xs = drop (length xs - n) xs

segmento n m xs = take (m-n+1) (drop (n-1) xs)

extremos n xs = (take n xs) ++ ( drop (length xs - n) xs)

dotprod xs ys = sum (zipWith (*) xs ys)

elemento x xs = any (==x) xs

somaQuadrados n = sum [x^2 | x <- [1 .. n]]

fatores n = [x | x <- [1 .. n], mod n x == 0]

perfeitos n = [x | x <- [1 .. n], sum (init (fatores x)) == x]

--interseccao xs ys = [x | x <- xs, y <- ys, x == y]

interseccao xs ys = [x | x <- xs, elem x ys]

diferenca xs ys = [x | x <- xs, notElem x ys]

gensquares a b = [x^2 | x <- [a .. b], mod x 2 == 0]

primo x = (fatores x == [1,x])

divide 0 _ = 0
divide 1 1 = 1
divide 1 _ = 0
divide _ 1 = 0
divide a b | mod b a == 0 = 1 + (divide a (div b a))
           | otherwise = 0

fatoresPrimos x = [(a,divide a x) | a <- [1 .. x], mod x a == 0, primo a]

multiplos x = [x * y | y <- [1 .. x]]

diamonds x = [ (multiplos y) | y <- ([1 .. x] ++ tail ([x, (x-1) .. 1]))]

-- terceira questão

--parcelasNumerador n = [4*(-1)^(x+1) | x <- [1 .. n]]

--numeradores = 4 : map (*(-1)) numeradores
--denominadores = 1 : map (+2) denominadores

numeradores = 4 : [x * (-1) | x <- numeradores]
denominadores = 1 : [x + 2 | x <- denominadores]

parcelas = zipWith (/) numeradores denominadores

calcPi1 n =  sum (take n parcelas)

--Retornar soma 10 primeiros elementos
-- 3.04183+96189294032
--Retornar soma 100 primeiros elementos
-- 3.1315929035585537
--Retornar soma 1000 primeiros elementos
-- 3.140592653839794

binom n k = (product [(k+1)..n] `div` (product [1..(n-k)]))

pascal n = [[binom y x | x <- [0 .. y]] | y <- [0 .. n]]

pascal2 = [[binom y x | x <- [0 .. y]] | y <- [0 ..]]
