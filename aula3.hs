sinal :: Int -> Int

sinal x | x > 0     = 1
        | x == 0    = 0
        | otherwise = -1

concatena :: [[a]] -> [a]
concatena [] = []
concatena (x:xs) = x ++ concatena xs

replica :: Int -> a -> [a]
replica 0 _ = []
replica n a = a:(replica (n-1) a)
--replica n a = [a] ++ (replica (n-1) a)

elemento :: Eq a => a -> [a] -> Bool
elemento _ [] = False
--elemento a (x:xs) = (a == x) || (elemento a xs)
elemento a (x:xs) | (a == x)  = True
                  | otherwise = (elemento a xs)

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) | x > y = False
                  | otherwise = isSorted (y:xs)

palindromo :: String -> Bool
palindromo [] = True
palindromo [x] = True
palindromo (x:xs) | x == (last xs) = palindromo (init xs)
                  | otherwise = False



rotEsq :: Int -> [a] -> [a]

rotEsq 0 xs = xs
rotEsq n xs = (drop n xs) ++ (take n xs)
-- Caso em que n é maior que length de xs
