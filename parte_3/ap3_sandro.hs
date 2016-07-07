descompacta :: [(a,b)] -> ([a],[b])

descompacta [] = ([],[])
descompacta ((a,b):xs) = ((a:as),(b:bs))
						where (as,bs) = descompacta xs 

type Item = String
total :: [(Item, Double)] -> [Item] -> Double

preco = [("Leite", 2.0), 
	("Manteiga", 2.5),
	( "Batata" , 4.0 ),
	("Brocolis" , 2.0),
	( "Cenoura" , 2.2)]

total [] _ = 0
total _ [] = 0
total p (x:xs) = (buscaPreco p x) + (total p xs)

buscaPreco [] _ = 0
buscaPreco ((a,b):as) x | a == x = b
						| otherwise = buscaPreco as x 


isPrefix [] _ = True
isPrefix xs [] = False
isPrefix (x:xs) (y:ys) = if x == y then (isPrefix xs ys) else False

subLista [] _ = True
subLista xs [] = False
subLista xs (y:ys) =  (isPrefix xs (y:ys)) || (subLista xs ys)

data Prop = And Prop Prop | Or Prop Prop | Not Prop | Val Bool

eval (Val p) = p
eval (Or p q) = (eval p) || (eval q)
eval (And p q) = (eval p) && (eval q)
eval (Not p) = not (eval p)

prop1 = (And (Val True) (Or (Val False) (Val True)))
prop2 = (Not (And (Val True) (Val False)))

data Arv a = Folha a | Ramo (Arv a) (Arv a) deriving (Show)

arv = (Ramo (Ramo (Folha 6) (Ramo (Ramo (Folha 5) (Folha 4)) (Folha 3))) (Ramo (Folha 1) (Folha 2)) )  

foldTree f1 f2 (Folha x) = f1 x
foldTree f1 f2 (Ramo a b) = f2 (foldTree f1 f2 a) (foldTree f1 f2 b)

data ArvBin a = Vazia | No a (ArvBin a) (ArvBin a) deriving (Eq, Show)

arv1 = No 4 
		(No 3 Vazia Vazia)
		(No 5 
			(No 6 Vazia Vazia)
			(No 4 Vazia Vazia)
		)

arv2 = No 7 
		(No 4 
			(No 5 Vazia Vazia)
			Vazia
		)
		(No 3
			(No 2 Vazia Vazia)
			Vazia
		)
 
zipAB Vazia _ = Vazia 
zipAB _ Vazia  = Vazia
zipAB (No x1 a1 b1) (No x2 a2 b2) = No (x1,x2) (zipAB a1 a2) (zipAB b1 b2)

data Estrada = Cidade String | Bifurcacao Estrada Estrada deriving (Show)

estrada = Bifurcacao 
			(Cidade "Quixada")
			(Bifurcacao
				(Bifurcacao
					(Cidade "Quixeramobim")
					(Cidade "Senador Pompeu")
				)
				(Cidade "Madalena")
			)

alcanca :: String -> Estrada -> Bool
alcanca x (Cidade c) | x == c = True
					 | otherwise = False
alcanca x (Bifurcacao e1 e2) = (alcanca x e1) || (alcanca x e2)  


data Direcao = Esq | Dir deriving (Show)
mapaRodoviario x (Cidade c) | x == c = Just []
							| otherwise = Nothing 
mapaRodoviario x (Bifurcacao esq dir) = case mapaRodoviario x esq of 
										Just xs -> Just (Esq: xs)
										Nothing -> case mapaRodoviario x dir of
												Just xs -> Just (Dir:xs)
												Nothing -> Nothing		

type Doc = [DocPart]

data DocPart = Texto String | Tag String Doc

pagina = 
	[ Texto "Bem-Vindo",
		Tag "P" [
			Tag "B" [
				Texto "Meus interesses sao programacao em ", 
				Tag "Em" [Texto "Haskell"],
				Texto " e assistir ",
				Tag "EM" [Texto "Prison Break"],
				Texto "."
				]
		],
		Tag "P" [ 
				Texto "Obrigado por visitar", 
				Tag "EM" [Texto "pedro@gmail.com"]
			]

	]

showDoc [] = ""
showDoc ((Texto s):xs) = s ++ (showDoc xs)
showDoc ((Tag s ys):xs) = "<" ++ s ++ ">"  ++ (showDoc ys) ++ "</" ++ s ++ ">" ++ (showDoc xs)