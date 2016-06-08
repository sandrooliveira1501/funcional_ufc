--Questões 1 ok,2 ok,3 ok,8 ok e 13 ok, 7 ok

import Data.List

-- 1
--De acordo com a questão
safeLog :: (Floating a, Ord a) => a -> Maybe a
safeLog x | x > 0 = Just (log x)
          | otherwise = Nothing

-- aceitando log de 0
--safeLog :: (Floating a, Ord a) => a -> Maybe a
--safeLog x | x >= 0 = Just (log x)
--          | otherwise = Nothing


-- 2

data Ponto = Pt Float Float
type Regiao = Ponto -> Bool
type Raio = Float

--a)
-- type Ponto = (Float, Float)
-- dist p1 p2 = sqrt ((( fst(p1) - fst(p2))^2) + ((snd(p1) - snd(p2))^2))

dist :: Ponto -> Ponto -> Float
dist (Pt x1 y1) (Pt x2 y2) = sqrt (((x1 - x2)^2) + ((y1 - y2)^2))

--b)
comprimento :: [Ponto] -> Float
comprimento [] = 0
comprimento [p] = 0
comprimento (p:p2:ps) = (dist p p2) + comprimento(p2:ps)

-- 3

circ1 = circ (Pt 0 0) 10
circ2 = circ (Pt 100 100) 10
rt1 = retang (Pt 15 15 ) (Pt 20 20)
rt2 = retang (Pt 18 18 ) (Pt 22 22)
rt3 = retang (Pt 18 18 ) (Pt 20 20)

--a)

retang:: Ponto -> Ponto -> Regiao
retang (Pt x1 y1) (Pt x2 y2) = (\(Pt x y) -> (x>= x1 && x <= x2) && (y >= y1 && y <= y2))

circ:: Ponto -> Raio -> Regiao
circ ponto r = (\(ponto2) -> (dist ponto ponto2) <= r )

--b)

uniao:: Regiao -> Regiao -> Regiao
uniao r r2 = (\ponto -> (r ponto) || (r2 ponto))

interseccao:: Regiao -> Regiao -> Regiao
interseccao r r2 = (\ponto -> (r ponto) && (r2 ponto))

--complemento r r2 = (\ponto -> (not (r ponto)) && (r2 ponto))  -- O complemento do conjunto r2 contido no conjunto r
complemento:: Regiao -> Regiao
complemento r = (\ponto -> not (r ponto))

-- 7

data MConj a = Vazio | No a Int (MConj a) (MConj a) deriving (Show)
mconj1 =  No 'A' 2 Vazio (No 'B' 1 Vazio Vazio)
mconj2 =  inserir 'C' mconj1
mconj3 =  No 3 2 Vazio (No 4 1 Vazio Vazio)
mconj4 =  inserir 1 mconj3

-- a)
ocorre :: Ord a => a -> MConj a -> Int
ocorre _ Vazio = 0
ocorre valor (No x n subA subB) = if (valor == x) then n else ((ocorre valor subA) + (ocorre valor subB))

--b)

inserir :: Ord a => a -> MConj a -> MConj a
inserir valor Vazio = No valor 1 Vazio Vazio
inserir valor (No x n subA subB) | valor == x = (No x (n+1) subA subB) 
                                 | valor < x =  (No x n (inserir valor subA) subB)
                                 | valor > x =  (No x n subA (inserir valor subB))

--c)

listar :: MConj a -> [a]
listar Vazio = []
listar (No x n subA subB) = (listar subA) ++ [x | _ <- [1 .. n]] ++ (listar subB) 

--d)

tamanho :: MConj a -> Int
tamanho Vazio = 0
tamanho (No x n subA subB) = n + (tamanho subA) + (tamanho subB)

--e)

--sumMConj :: MConj Int -> Int
--sumMConj Vazio = 0
--sumMConj (No x n subA subB) = (x * n) + (sumMConj subA) + (sumMConj subB)
sumMConj :: Num a => MConj a -> a
sumMConj conj = sum (listar conj)


-- 8

data Mobile = Haste Mobile Int Mobile Int | Objeto Int deriving (Eq, Ord)

m1 = Haste ( Objeto 1) 6 ( Objeto 3) 2
m2 = Haste ( Objeto 2) 4 ( Objeto 4) 2
m3 = Haste ( Objeto 1) 1 ( Objeto 1) 1
m4 = Haste (m3) 3 (m2) 1
m5 = Haste (m4) 2 (m1) 6 -- não está de acordo com a figura
m6 = Haste (m4) 2 (m1) 4 -- de acordo com a figura

-- a
peso :: Mobile -> Int
peso (Objeto p) = p
peso (Haste sub1 x sub2 y) = (peso sub1) + (peso sub2)

--b
equilibrio :: Mobile -> Bool
equilibrio (Objeto x) = True
equilibrio (Haste sub1 x sub2 y) = if (x * p1) == (y * p2) then (equilibrio sub1) && (equilibrio sub2)  else False
                            where
                                p1 = peso sub1
                                p2 = peso sub2


-- Definicação de tipo Prop

data Prop = Const Bool | Var Char | Neg Prop | Conj Prop Prop | Disj Prop Prop | Impl Prop Prop deriving (Eq , Show)


prop1 = Impl ( Var 'P' ) ( Var 'Q' )
prop2 = Impl (Neg ( Var 'P' ) ) ( Var 'Q' )
prop3 = Impl prop1 prop2

variaveis :: Prop -> [Char]
variaveis ( Const x ) = []
variaveis ( Var x ) = [x]
variaveis (Neg p ) = variaveis p
variaveis ( Conj p q ) = nub ( variaveis p ++ variaveis q )
variaveis ( Disj p q ) = nub ( variaveis p ++ variaveis q )
variaveis ( Impl p q ) = nub ( variaveis p ++ variaveis q )

-- 13

showProp:: Prop -> String
showProp (Const x) = if (x) then "T" else "F"
-- showProp (Const x) = (show x)
showProp (Var x) = [x]
showProp (Neg p) = "(~" ++ (showProp p) ++ ")"
showProp ( Conj p q ) = "(" ++ (showProp p) ++ " && " ++ (showProp q) ++ ")"
showProp ( Disj p q ) = "(" ++ (showProp p) ++ " || " ++ (showProp q) ++ ")"
showProp ( Impl p q ) = "(" ++ (showProp p) ++ " -> " ++ (showProp q) ++ ")"
