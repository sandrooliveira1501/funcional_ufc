
module Main (main) where

--main = do 
--		n <- readLn
--		elefantes n

--main = somaN


seq_ [] = return ()
seq_ (x:xs) = x >> seq_ xs


elefantes n | n <= 2 = return ()
            | otherwise = do
                elefantes (n-1)
                putStrLn ("Se " ++ show (n-1) ++ " elefantes incomodam muita gente, ")
                putStrLn ((show n) ++ " elefantes incomodam muita mais!")

somaN = do 
        n <- readLn
        x <- soma n
        putStrLn (show x)

soma :: Int -> IO Int
soma 0 = return 0
soma n = do 
        x <- readLn
        y <- soma (n-1)
        return (x + y)

--soma2 :: Int -> IO [Int]
soma2 0 = return []
soma2 n = do 
            x <- getLine
            xs <- (soma2 (n-1))
            return ([x] ++ xs)

-- 4
exemploInteract = interact (show.length.lines)

inverteInteract = interact (unlines.(map (\x -> reverse x)).lines)

-- 5

palavras [] = 0
palavras (x:xs) = (palavrasAux x) + (palavras xs)

palavrasAux "" = 0 
palavrasAux x   | head x == ' ' = palavrasAux (dropWhile (== ' ') x)
                | otherwise = 1 + (palavrasAux (dropWhile (/= ' ') x) )


wc = do
        x <- getContents
        putStrLn ("Linhas: " ++ (show (length (lines x))))
        putStrLn ("Palavras: " ++ (show (palavras (lines x))))
        putStrLn ("Linhas: " ++ (show (length x)))

--main = wc

fatorial 0 = 1
fatorial n = n * (fatorial (n-1))
imprimeFat = do 
                x <- readLn
                putStrLn $ show $ fatorial x

--main = imprimeFat

divisores n = [x | x <- [1 .. n], mod n x == 0]

ehPrimo n = (divisores n) == [1,n] 

imprimePrimo = do 
                x <- readLn
                putStrLn $ show $ ehPrimo x

--main = imprimePrimo

accumulate :: [IO a] -> IO [a]
accumulate [] = return []
accumulate (x:xs) = do 
                    a <- x
                    as <- accumulate xs
                    return (a:as)

{- main = do 
        x <- accumulate [getChar | x <- [1..10]]
        putStrLn x -} 

-- questão 9 

accumulateSum = do 
                n <- readLn
                xs <- accumulate [readLn | x <- [1..n]]
                putStrLn $ show $ sum xs

main = accumulateSum