data Arv a = Vazia | No a (Arv a) (Arv a) deriving (Eq, Show)

arv1 = Vazia
arv2 = No 2 arv1 arv1
arv3 = No 4 arv2 arv1
arv4 = No 5 arv3 arv2

tamanhoArv Vazia = 0
tamanhoArv (No x esq dir) = 1 + (tamanhoArv esq) + (tamanhoArv dir)

alturaArv Vazia = 0
alturaArv (No x esq dir) = 1 + (max (alturaArv esq) (alturaArv dir))

type Ponto = (Float, Float)
dist p1 p2 = sqrt ((( fst(p1) - fst(p2))^2) + ((snd(p1) - snd(p2))^2))