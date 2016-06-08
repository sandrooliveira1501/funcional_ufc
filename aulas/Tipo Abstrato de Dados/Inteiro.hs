module Inteiro where

data Inteiro = Zero | Succ Inteiro | Pred Inteiro deriving (Show)

toInt :: Inteiro -> Integer
toInt Zero     = 0
toInt (Succ x) = toInt x + 1
toInt (Pred x) = toInt x - 1

fromInt :: Integer -> Inteiro
fromInt 0 = Zero
fromInt x | x > 0     = Succ ( fromInt (x-1) )
		  | otherwise = Pred ( fromInt (x+1) )

--instance Show Inteiro where
--	show x = show (toInt x)

soma :: Inteiro -> Inteiro -> Inteiro
soma x Zero     = x
soma x (Succ y) = soma (Succ x) y
soma x (Pred y) = soma (Pred x) y

neg :: Inteiro -> Inteiro
neg Zero = Zero
neg (Succ x) = Pred (neg x)
neg (Pred x) = Succ (neg x)

mul :: Inteiro -> Inteiro -> Inteiro
mul x Zero     = Zero
mul x (Succ y) = soma x (mul x y)
mul x (Pred y) = soma (neg x) (mul x y)

positivoAux Zero y     = y > 0
positivoAux (Succ x) y = positivoAux x (y+1)
positivoAux (Pred x) y = positivoAux x (y-1)

positivo :: Inteiro -> Bool
positivo x = positivoAux x 0


--subProp :: Inteiro -> Inteiro -> Inteiro
--subProp x Zero       | positivo x = x
--		             | otherwise  = Zero
--subProp x (Succ y)	= subProp (Pred x) y
--subProp x (Pred y)  = subProp (Succ x) y 	       
    
--absolute :: Inteiro -> Inteiro                     
--absolute x = soma (subProp x Zero) (subProp (neg x) Zero ) 

absolute x = if positivo x then x else (neg x)

sinal Zero = Zero
sinal x    | positivo x = Succ Zero
            | otherwise  = Pred Zero 

instance Num Inteiro where
	z1 + z2       = soma z1 z2
	z1 * z2       = mul z1 z2
	negate z      = neg z
	abs z         = absolute z
	signum z      = sinal z
	fromInteger z = fromInt z   



n0 = Zero
n1 = Succ n0
n2 = Succ n1
n3 = Succ n2
n4 = neg n3


