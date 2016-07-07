1--
descompacta [(1,2),(3,4),(5,6),(4,5)] == ([1,3,5,4],[2,4,6,5])
descompacta [(1,2),(3,4),(5,6),(4,5),(5,6)] == ([1,3,5,4,5],[2,4,6,5,6])

2--
type Item = String
total :: [(Item,Double)] -> [Item] -> Double

preco =
	[("Leite",2.0),
	("Manteiga", 2.5),
	("Batata",4.0),
	("Brocolis", 2.0),
	("Cenoura",2.2)
	]

*Main> total preco ["Cenoura"]
2.2
*Main> total preco ["Cenoura","Leite"]
4.2
*Main> total preco ["Cenoura","Leite","Leite"]
6.2

data Prop
	= And Prop Prop
	| Or Prop Prop
	| Not Prop
	| Val Bool

prop1 = (And (Val True) (Or (Val False) (Val True)))
prop2 = (Not (And (Val True) (Val False)))

*Main> eval prop1
True
*Main> eval prop2
True

data ArvBin a
  = Vazia
  | No a (ArvBin a) (ArvBin a)
  deriving (Eq,Show)
arv1 =
  No 4
    (No 3 Vazia Vazia)
    (No 5
        (No 6 Vazia Vazia)
        (No 4 Vazia Vazia)
    )
arv2 =
  No 7
    (No 4
        (No 5 Vazia Vazia)
        Vazia
    )
    (No 3
        (No 2 Vazia Vazia)
        Vazia
    )

*Main> zipAB arv1 arv2
No (4,7) (No (3,4) Vazia Vazia) (No (5,3) (No (6,2) Vazia Vazia) Vazia)


data Estrada
	= Cidade String
	| Bifurcacao Estrada Estrada

estrada :: Estrada
estrada =
	Bifurcacao
		( Cidade "Quixada" )
		( Bifurcacao
			( Bifurcacao
				( Cidade "Quixeramobim" )
				( Cidade "Senador Pompeu" )
			)
		( Cidade "Madalena" )
	)

*Main> alcanca "Quixada" estrada
True
*Main> alcanca "Quixeramobim" estrada
True
*Main> alcanca "Pedra Branca" estrada
False


data Direcao = Esq | Dir deriving (Show)


*Main> mapaRodoviario "Quixada" estrada
Just [Esq]
*Main> mapaRodoviario "Quixeramobim" estrada
Just [Dir,Esq,Esq]
*Main> mapaRodoviario "Senador Pompeu" estrada
Just [Dir,Esq,Dir]


type Doc = [DocPart]
data DocPart = Texto String
    | Tag String Doc


pagina :: Doc
pagina =
	[ Texto "Bem-vindo a minha pagina!"
	, Tag "P" [ Tag "B" [ Texto "Meus interesses sao programacao em"
	                    , Tag "EM" [ Texto "Haskell" ]
	                    , Texto " e assistir "
	                    , Tag "EM" [ Texto "Prison Break" ]
	                    , Texto "."
	                    ] ]
	, Tag "P" [ Texto "Obrigado por visitar!"
	          , Tag "EM" [ Texto "pedro@gmail.com" ]
	          ]
	]

*Main> showDoc pagina
"Bem-vindo a minha pagina!<P><B>Meus interesses sao programacao em <EM>Haskell</
EM> e assistir <EM>Prison Break</EM>.</B></P><P>Obrigado por visitar!<EM>pedro@g
mail.com</EM></P>"
