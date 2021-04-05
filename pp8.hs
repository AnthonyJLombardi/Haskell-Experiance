--I was not in class on 9/15/20
ziprec :: [a] -> [b] -> [(a,b)]
ziprec [] _ = []
ziprec _ [] = []
ziprec (x:xs) (y:ys) = (x, y) : ziprec xs ys
