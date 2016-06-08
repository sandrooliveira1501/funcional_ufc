import Prelude hiding (repeat, cycle, iterate)
nats :: [Int]
nats = [0..]

pares :: [Int]
pares = [0,2..]

uns :: [Int]
uns = 1 : uns

ints :: Int -> [Int]
ints n = n : ints (n+1)

repeat :: a -> [a]
repeat x = x : repeat x

cycle :: [a] -> [a]
cycle [] = error "cycle : empty list"
cycle xs = xs ++ cycle xs

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

primos :: [Integer]
primos = crivo [2..]

crivo :: [Integer] -> [Integer]
crivo (p:xs) = p : crivo [ x | x <- xs, mod x p /= 0]

raiz :: Double -> [Double]
raiz q = iterate (\x -> 0.5*(x + q/x)) q

absolute xs eps = head [x | (x,x') <- zip xs (tail xs), abs(x-x')<eps]
