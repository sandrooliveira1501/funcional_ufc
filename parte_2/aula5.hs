uns :: [Int]
uns = 1 : uns

ints :: Int -> [Int]
ints n = n:(ints (n+1))

diferenca xs ys = [x | x <- xs, notElem x ys]

ciclo xs = [x | _ <- [1 ..],x <- xs]

iterador f x = x:[f y | y <- (iterador f x)]

preencherE :: Int -> String -> String
preencherE n s = (take (n - length s) (repeat ' ')) ++ s

aproximacaoRaiz t = iterate (\x -> 0.5*(x+(t/x))) t

absoluto xs epsilon = head [x | (x,y) <- zip xs (tail xs), abs (x-y) <= epsilon]

relativo xs epsilon = head [x | (x,y) <- zip xs (tail xs), abs ((y - x) / x) <= epsilon]

fib = 0:1:zipWith (+) fib (tail fib)

primos = crivo [2 ..]

crivo (x:xs) = x:crivo[y | y <- xs, mod y x /= 0]

fat 0 = 1
fat x = x * fat (x-1)

binom n k = (product [(k+1)..n] `div` (product [1..(n-k)]))

pascal = [[binom x y | y <- [0 .. x]] | x <- [0 ..] ]

pascal2 = [[ if (y == 0 || y == x) then 1 else ((pascal !! (x-1) !! y) + (pascal !! (x-1) !! (y-1))) | y <- [0 .. x]] | x <- [0 ..] ]
