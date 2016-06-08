inserir x [] = [x]
inserir x (y:ys) | x < y = x:y:ys
                 | otherwise = y: (inserir x ys)

insertion [] = []
insertion (x:xs) = inserir x (insertion xs)
