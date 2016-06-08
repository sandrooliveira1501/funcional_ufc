import Prelude hiding (product, length, reverse, (++), zip, drop, init)
fatorial 0 = 1
fatorial n = n*fatorial(n-1)

fatorial2 n = fatorialTail (n,1)
	where
		fatorialTail (0,y) = y
		fatorialTail (n,y) = fatorialTail(n-1,n*y)

product [] = 1
product (x:xs) = x*product xs

length []     = 0
length (_:xs) = 1 + length xs

reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

zip [] _  = []
zip _  [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys 

drop 0 xs     = xs
drop n []     = []
drop n (x:xs) | n > 0 = drop (n-1) xs

quicksort [] = []
quicksort (x:xs) = quicksort menores ++ [x] ++ quicksort maiores
	where
		menores = [ y | y <- xs, y <= x]
		maiores = [ y | y <- xs, y > x ]

init []     = error "init : empty list"
init [x]    = []
init (x:xs) = x : init xs



