-- Lista 4

-- Questão 3

partir n xs | length xs > n = (take n xs) : (partir n (drop n xs))
						| otherwise = [xs]

-- Questão 5

--a)
findIn x xs = takeWhile (==x) (xs)
--b)
removeIn x xs = dropWhile (==x) (xs)
--c)
group [] = []
group (x:xs) = (x, length (findIn x (x:xs))): group (removeIn x (x:xs))

-- Questão 7

--a)
relacionados p [] = True
relacionados p [x] = True
relacionados p (x:y:ys) | p x y = relacionados p (y:ys)
												| otherwise = False

--b)
relacionados2 p xs = and [ (p (xs !! x ) (xs !! (x+1))) | x <- [0 .. (length xs - 2)]]

relacionados3 p xs = and [p x y | (x,y) <- zip xs (tail xs)]

-- Questão 16

--a
filtraAplicaCL f p xs = [f x | x<-xs, p x]

--b
filtraAplicaMF f p xs = map f (filter p xs)

--c
filtraAplica f p [] = []
filtraAplica f p (x:xs) | p x = (f x):(filtraAplica f p xs)
			| otherwise = filtraAplica f p xs

--d
filtraAplicaFold f p xs = foldr (\x a-> if p x then (f x):a else a) [] xs

-- Questão 17

horner [] z = 0
horner (c:cs) z = c + z * (horner cs z)

hornerFold cs z = foldr (\x a -> x + z * a) 0 cs

-- Questão 18

mapFold f xs = foldr (\x a -> (f x): a ) [] xs

-- Questão 19

filterFold p xs = foldr (\x a -> if p x then x:a else a) [] xs

-- Questão 23
inserir x [] = [x]
inserir x (y:ys) | x < y = x:y:ys
                 | otherwise = y: (inserir x ys)

insertionFold xs = foldr (\x a -> inserir x a) [] xs
