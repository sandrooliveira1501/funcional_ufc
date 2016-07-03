module Main (main) where

import Data.Char (isDigit)
import System.Random
import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

main = do 
		hSetBuffering stdout NoBuffering
		x <- randomRIO (1,100) :: IO Int
		--x <- return 1
		n <- jogo 1 x
		putStrLn ("acertou em " ++ show n ++ " tentativas")

jogo n x = do 
	 putStr "Tentativa? "
	 y <- readLn
	 if(y == x) then return n else do
	 	 putStrLn (show y)
	 	 putStrLn (altoOuBaixo y x) 
	 	 jogo (n+1) x

altoOuBaixo y x | y < x = "Baixo"
				| otherwise = "Alto"