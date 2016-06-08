module Inteiro (Inteiro, toInt, fromInt) where

data Inteiro = Zero | Succ Inteiro | Pred Inteiro

toInt :: Inteiro -> Integer
toInt Zero     = 0
toInt (Succ x) = toInt x + 1
toInt (Pred x) = toInt x - 1

fromInt :: Integer -> Inteiro
fromInt 0 = Zero
fromInt x | x > 0     = Succ (fromInt (x-1))
		  | otherwise = Pred (fromInt (x+1))

instance Show Inteiro where
	show x = show (toInt x)

instance Num Inteiro where
	z1 + z2       = fromInt ( (toInt z1) + (toInt z2) )
	z1 * z2       = fromInt ( (toInt z1) * (toInt z2) )
	negate z      = fromInt ( negate (toInt z) )
	abs z         = fromInt ( abs (toInt z) )
	signum z      = fromInt ( signum (toInt z) )
	fromInteger z = fromInt z

instance  Eq Inteiro where
	z1 == z2 = (toInt z1) == (toInt z2) 

instance Ord Inteiro where
	z1 <= z2 = z1 == z2 || (toInt z1) < (toInt z2)
