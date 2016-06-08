import Prelude hiding ((||))


minusculas :: Char -> Bool
minusculas c = c >= 'a' && c <= 'z'

fact :: Int -> Int
fact n = product [1..n]

abs :: Float -> Float
abs x | x >= 0    = x
	  | otherwise = -x

sinal :: Int -> Int
sinal x | x == 0 = 0
        | x > 0  = 1
        | x < 0  = -1

raizes :: Float -> Float -> Float -> [Float]
raizes a b c | delta >  0 = [(-b + sqrt delta)/(2*a),
                             (-b - sqrt delta)/(2*a)]
             | delta == 0 = [-b/(2*a)]
             | otherwise  = []
             where 
             	delta = b*b - 4*a*c  
raizes2 :: Float -> Float -> Float -> [Float]
raizes2 a b c
	| delta > 0 = 	let r = sqrt delta 
					in [(-b+r)/(2*a), (-b-r)/(2*a)]
	| delta == 0 = [-b/(2*a)]
	| otherwise  = []
	where 
		delta = b^2 - 4*a*c


(||) :: Bool -> Bool -> Bool
False || x = x
True  || _ = True


--operador lógico xor
xor :: Bool -> Bool -> Bool
xor x y | x == y = False
        | otherwise = True


--impares n = devolve uma lista com n primeiros ímpares
impares n = map f [0..(n-1)]
			where f x = 2*x + 1

--somaIntervalo a b = \sum_{i = a}^{b} i
somaIntervalo a b = sum [a..b]

