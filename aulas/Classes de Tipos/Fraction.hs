module Fraction(Fraction) where

import GHC.Real

data Fraction = Q Integer Integer

instance  Show Fraction where
	show (Q x y) = show x ++ "/" ++ show y ++ "(" ++ show ( (fromIntegral x) / (fromIntegral y) ) ++ ")"
				
instance Num Fraction where
	(Q x y) + (Q z w) = Q (x*w + y*z) (y*w)
	(Q x y) * (Q z w) = Q (x*z) (y*w)
	negate (Q x y)  = Q (negate x) y
	abs    (Q x y)  = Q (abs x) y
	signum (Q x y)  = Q (signum x) 1
	fromInteger z   = Q z 1

instance Fractional Fraction where
	f1 / f2 = f1 * (recip f2) 
 	recip (Q a b)  = Q b a
	fromRational r = Q (numerator r) (denominator r)

instance Eq Fraction where
	(Q a b) == (Q c d) = a*d == b*c

instance Ord Fraction where
	(Q a b) <= (Q c d) = a*d <= b*c 