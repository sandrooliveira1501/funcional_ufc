import Prelude hiding (map, filter, takeWhile, dropWhile, all, any, sum, product, and, or, length)

twice :: (a -> a) -> a -> a
twice f x = f (f x)

map :: (a -> b) -> [a] -> [b]
map f [] 	 = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs) | p x 		= x : filter p xs
				| otherwise = filter p xs

takeWhile, dropWhile :: (a -> Bool) -> [a] -> [a]

takeWhile p []     = []
takeWhile p (x:xs) 	| p x = x : takeWhile p xs
					| otherwise = []

dropWhile p []     = []
dropWhile p (x:xs) 	| p x = dropWhile p xs
					| otherwise = (x:xs)

all p xs = and (map p xs)
any p xs = or (map p xs)

sum = foldr (+) 0 
product = foldr (*) 1
and = foldr (&&) True
or  = foldr (||) False
length = foldr (\x n-> n + 1) 0