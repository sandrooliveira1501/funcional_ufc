--factorial 0 = 1
--factorial n = n * factorial(n-1)


-- Lista 3

--1 - a) remove :: Eq a => a -> [a] -> [a]

--b) partes :: [a] -> [[a]]


--p)
  remove _ [] = []
  remove x (y:ys) | x == y = remove x ys
                  | otherwise = (y:(remove x ys))

  unique [] = []
  unique (x:xs) = x: unique (remove x xs)

  inserir x [] = [x]
  inserir x [y] | x > y = [y] ++ [x]
                | otherwise = x:[y]

  inserir x (a:b:ys) | x <= a = x:a:b:ys
                     | x > a && x <=b = a:x:b:ys
                     | otherwise = a: inserir x (b:ys)


  inserir x [] = [x]
  inserir x (y:ys) | x < y = x:y:ys
                    | otherwise = y: (inserir x ys)
--
