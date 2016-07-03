module Main (main) where

main = do
	putStrLn "Digite uma sequência de números:"
	putStrLn "Para terminar digite o valor zero"
	soma <- lerESomar
	putStrLn "A soma dos números digitados é "
	putStrLn (show soma)

lerESomar = do
	n <- readLn
	if n == 0 then return 0
	else do
		somaResto <- lerESomar
		return (n + somaResto)