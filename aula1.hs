dobro :: Num a => a -> a
dobro x = 2*x

divisores n = [x | x <- [1..n], mod n x == 0]

divisoresComum a b = [x | x <- [1..a], mod a x == 0, mod b x == 0]

primo :: Integral a => a -> Bool
primo n = length (divisores (n)) == 2

mdc a b = maximum (divisoresComum a b)

final :: Int -> [a] -> [a]
final n xs= drop (length (xs) - n) xs

rota a xs= drop a xs ++ take a xs

rotaD a xs= drop (length(xs) - a) xs ++ take (length(xs) - a) xs

ordenado:: [Int] -> Bool
ordenado [] = True
ordenado [x] = True
ordenado (x:y:xs) = x <= y && ordenado(y:xs)

tamanho :: [a] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

pegar :: Int -> [a] -> [a]
pegar _ [] = []
pegar 0 _  = []
pegar n (x:xs) = x:(pegar (n-1) xs)

apaga :: Int -> [a] -> [a]

apaga _ [] = []
apaga 0 xs = xs
apaga n (x:xs) = apaga (n-1) xs
