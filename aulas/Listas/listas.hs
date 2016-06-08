import Prelude hiding (concat)
import Data.Char
quadrados :: Int -> [Int]
quadrados n = [x^2 | x <- [1..n]]

produtoCartesiano :: Int -> Int -> [(Int,Int)]
produtoCartesiano a b = [ (x,y) | x <- [1..a], y <-[1..b]]

concat :: [[a]] -> [a]
concat xss = [ x | xs <- xss, x <- xs]

divisores n = [ x | x <- [1..n], mod n x == 0]

primo n = divisores n == [1,n]

primos n = [x | x <- [2..n], primo x]

pares :: [a] -> [(a,a)]
pares xs = zip xs (tail xs)

crescente xs = and [ x <= y | (x,y) <- pares xs]

indices :: Eq a => a -> [a] -> [Int]
indices x ys = [ i | (i,y) <- zip [0..n] ys, x == y]
	where 
		n = length ys - 1

stringUpper :: String -> String
stringUpper cs = [toUpper c | c <- cs]