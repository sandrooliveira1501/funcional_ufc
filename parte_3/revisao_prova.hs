data Polinomio a = PolZero | ConsPol a Int (Polinomio a) deriving Eq

pol1 = ConsPol 4 2 (ConsPol 5 1 (ConsPol 4 0 PolZero))
pol2 = ConsPol 4 3 (ConsPol 5 2 (ConsPol 4 0 PolZero))
vazio = PolZero

isZero PolZero = True
isZero _ = False

instance Show a => Show (Polinomio a) where
        show PolZero = "0"
        show (ConsPol c x pol) = if (isZero pol) then s2 else s1 ++ (show pol)
        						where 
        							s1 = "(" ++ (show c) ++ ")" ++ "x^" ++ (show x) ++ " + "
        							s2 = "(" ++ (show c) ++ ")" ++ "x^" ++ (show x)	


eval PolZero _ = 0

eval (ConsPol c x pol) val = c * (val ^ x) + (eval pol val)

adicionar c x PolZero = ConsPol c x PolZero
adicionar c x (ConsPol c' x' p) | x == x' = ConsPol (c+c') x p
								| x > x' = ConsPol c x (ConsPol c' x' p)
								| otherwise = ConsPol c' x' (adicionar c x p)  

soma PolZero q = q
soma p PolZero = p
soma (ConsPol c x p) q = adicionar c x (soma p q) 