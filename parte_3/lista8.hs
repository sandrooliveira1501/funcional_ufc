--monadas

module Main (main) where

--main = do 
--		n <- readLn
--		elefantes n	
--elefantes :: Int -> IO ()
 
-- main = somaLinhas

--main = imprimeFatorial

main = primo
-- 2
elefantes n = 
			if n <= 2 then 
				do
				putStrLn ""
			else do
				elefantes(n-1)
				putStrLn ((show (n-1) ) ++ " elefantes incomodam muita gente, ")
				putStrLn ((show n ) ++ " elefantes incomodam muito mais!")

-- 3

soma [] = 0 
soma (x:xs) = (read x) + (soma xs)

somaLinhas
	= do {
		tests <- getLine;
		contents <- getContents;
		putStrLn $ show $ soma $ take ( read tests ) ( lines contents )
	}

-- 6

fatorial 0 = 1
fatorial n = n * fatorial (n-1)

imprimeFatorial = do 
					n <- readLn
					putStrLn (show (fatorial n))


-- 7 

divisores n = [x | x<- [1 .. n], mod n x == 0 ]
ehPrimo n = (divisores n) == [1,n] 

primo = do 
			n <- readLn
			if (ehPrimo n) then putStrLn "SIM" else putStrLn "NÃO" 

-- entrada e saída
-- 1,3,4,5

-- 1

type Passaros = Int
type Barra = (Passaros, Passaros)

pousoEsq n (esq,dir)
			| abs ((esq + n) - dir) < 4 = Just(esq+n, dir)
			| otherwise = Nothing


pousoDir n (esq,dir)
			| abs (esq - (dir+n)) < 4 = Just(esq, dir+n)
			| otherwise = Nothing

data Pos = Esq | Dir deriving (Eq,Show)
type Pouso = (Pos, Passaros)

b1 = (0,0) :: Barra
p1 = (Esq,2) :: Pouso
p2 = (Dir,4) :: Pouso
sq = [p1,p2]

rotina :: Barra -> [Pouso] -> Maybe Barra

rotina b [] = Just b
rotina b ((x,y):xs) = 
				if(x == Esq) then do
					bn <- pousoEsq y b
					rotina bn xs
				else do
					bn <- pousoDir y b
					rotina bn xs

-- 3

(!?) :: [a] -> Int -> Maybe a
(!?) [] _ = Nothing
(!?) xs n = if n < 0 || n >= l then Nothing
			else Just (xs !! n) 
			where l = length xs

-- 4

swapAux i x j y ks = [if k == i then y else if k == j then x else ks !! k | k <- [0 .. ((length ks) -1)]]

swap :: Int -> Int -> [a] -> Maybe [a]
swap i j xs = do
				xi <- xs !? i
				xj <- xs !? j
				Just (swapAux i xi j xj xs)

-- 5

getElts :: [Int] -> [a] -> Maybe [a]

getElts is xs = mapM (\a -> xs !? a) is