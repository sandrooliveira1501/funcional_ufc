--Alexsandro Oliveira
--Mauro Roberto

--questão 10
concatenaFold xs = foldr (\x a -> x ++ a) [] xs

--questao11
invertFold xs = foldr (\x a -> a++[x]) [] xs

--questao12
tamanhoFold xs = foldr (\x n -> n+1) 0 xs

--questao13
elementoFold n xs = foldr (\x a -> (x == n) || a) False xs

--questao14
paridadeFold xs = foldr (\x a -> if x then not a else a) False xs

paridade [] = False
paridade (x:xs) | x = not (paridade xs)
		| otherwise = paridade xs

--questao15
duplicarFold xs = foldr (\x a -> if elem x vogais then x:x:a else x:a) [] xs
			where vogais = ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U']

duplicar [] = []
duplicar (x:xs) | elem x vogais = x:x:(duplicar xs)
		| otherwise = x:(duplicar xs)
			where vogais = ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U']

--questao16
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
