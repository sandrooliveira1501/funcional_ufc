type Passaros = Int
type Barra = (Passaros, Passaros)

pousoEsq :: Passaros -> Barra -> Maybe Barra
pousoEsq n (esq,dir) 
					| abs ((esq + n) - dir) < 4 = Just (esq + n, dir)
					| otherwise = Nothing

pousoDir :: Passaros -> Barra -> Maybe Barra
pousoDir n (esq,dir)
			| abs (esq - (dir + n)) < 4 = Just (esq, dir + n)
			| otherwise = Nothing

data Pos = Esq | Dir deriving (Eq, Show)
type Pouso = (Pos, Passaros)

b1 = (0,0) :: Barra
p1 = (Esq, 2) :: Pouso
p2 = (Dir, 4) :: Pouso
sq = [p1,p2]

rotina :: Barra -> [Pouso] -> Maybe Barra

rotina (x,y) [] = Just (x,y)
rotina (x,y) ((pouso,val):as) = do 
							(a,b) <- if pouso == Dir then pousoDir val (x,y) else pousoEsq val (x,y)
							rotina (a,b) as

data Expr = Number Integer | Neg Expr | Plus Expr Expr | Minus Expr Expr | Times Expr Expr | Div Expr Expr | Mod Expr Expr

expr1 = Plus (Neg (Number 2)) (Number 10)
expr2 = Div expr1 (Number 0)

eval1 :: Expr -> Maybe Integer
eval1 (Number x) = Just x
eval1 (Neg expr) = do 
					x <- eval1 expr
					Just (negate x)
eval1 (Plus e1 e2) = do
					x <- eval1 e1
					y <- eval1 e2
					Just (x + y) 
eval1 (Minus e1 e2) = do 
					x <- eval1 e1
					y <- eval1 e2
					Just (x - y)
eval1 (Times e1 e2) = do
					x <- eval1 e1
					y <- eval1 e2
					Just (x * y)
eval1 (Div e1 e2) =  do
					x <- eval1 e1
					y <- eval1 e2
					if(y == 0) then Nothing else Just (div x y)
eval1 (Mod e1 e2) =  do
					x <- eval1 e1
					y <- eval1 e2
					if(y == 0) then Nothing else Just (mod x y)

eval2 :: Expr -> [Integer]
eval2 (Number x) = [x]
eval2 (Neg expr) = do 
					x <- eval2 expr
					[negate x]
eval2 (Plus e1 e2) = do
					x <- eval2 e1
					y <- eval2 e2
					[x + y] 
eval2 (Minus e1 e2) = do 
					x <- eval2 e1
					y <- eval2 e2
					[x - y]
eval2 (Times e1 e2) = do
					x <- eval2 e1
					y <- eval2 e2
					[x * y]
eval2 (Div e1 e2) =  do
					x <- eval2 e1
					y <- eval2 e2
					if(y == 0) then [] else  [div x y]
eval2 (Mod e1 e2) =  do
					x <- eval2 e1
					y <- eval2 e2
					if(y == 0) then [] else  [mod x y]

eval3 :: Monad m => Expr -> m Integer
eval3 (Number x) = return x
eval3 (Neg expr) = do 
					x <- eval3 expr
					return (negate x)
eval3 (Plus e1 e2) = do
					x <- eval3 e1
					y <- eval3 e2
					return (x + y) 
eval3 (Minus e1 e2) = do 
					x <- eval3 e1
					y <- eval3 e2
					return (x - y)
eval3 (Times e1 e2) = do
					x <- eval3 e1
					y <- eval3 e2
					return (x * y)
eval3 (Div e1 e2) =  do
					x <- eval3 e1
					y <- eval3 e2
					if(y == 0) then fail "Divisão por 0" else  return (div x y)
eval3 (Mod e1 e2) =  do
					x <- eval3 e1
					y <- eval3 e2
					if(y == 0) then fail "Divisão por 0" else  return (mod x y)

(!?) :: [a] -> Int -> Maybe a
(!?) [] _ = Nothing
(!?) xs n = if (length xs) <= n || n < 0 then Nothing else Just (xs !! n)

swap :: Int -> Int -> [a] -> Maybe [a]

swap x y [] = Nothing 
swap x y as = if x < 0 || y < 0 || x >= (length as) || y >= (length as) 
				then Nothing else Just (swapAux x y as 0)

swapAux x y as i | i == (length as) = []
				 | otherwise = if(i == x) then (as !! y):(swapAux x y as (i+1)) 
				 	else if(i == y) then (as !! x):(swapAux x y as (i+1)) 
				 	else (as !! i):(swapAux x y as (i+1))