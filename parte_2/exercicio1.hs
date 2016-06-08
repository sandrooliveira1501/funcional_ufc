
fat 0 = 1
fat x = x * fat (x-1)

binom n k = (product [(k+1)..n] `div` (product [1..(n-k)]))

pascal = [[binom x y | y <- [0 .. x]] | x <- [0 ..] ]

pascal2 = [[ if (y == 0 || y == x) then 1 else ((pascal2 !! (x-1) !! y) + (pascal2 !! (x-1) !! (y-1))) | y <- [0 .. x]] | x <- [0 ..] ]
