h x y = if x>y then x-y else y-x
  --  b) h :: (Num a, Ord a) => a -> a -> a

--f :: [Float] -> [Float] -> Float
--f :: ([Float],[Float]) -> Float
--f xs ys = sum [x*y | (x,y) <- zip xs ys]

f xs = reverse xs == xs

g [] = []
g [x] = [x]
g (x:_:xs) = x : g xs

z x = x * 2


segundo xs = head (tail xs)

trocar (x,y) = (y,x)

par x y = (x,y)

dobro x = 2 * x

metade x = x/2

minuscula x = x >= 'a' && x <= 'z'

intervalo x a b = x >= a && x <= b

ultimo xs = head (reverse xs)

ultimo2 xs = (!!) xs (length xs - 1)

triangulo a b c =  a < (b + c) && b < (a + c) && c < (a + b)

inserir x [] = [x]
inserir x (y:ys) | x < y = x:y:ys
                 | otherwise = y:(inserir x ys)



count p [] = 0
count p (x:xs) | p x = 1 + (count p xs)
               | otherwise = count p xs


count2 p xs = length (filter p xs)
