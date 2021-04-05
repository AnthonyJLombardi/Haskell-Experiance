--I was not in class on 9/8/20
remove :: Ord a => a -> [a] -> [a]
remove _ [] = []
remove e (x:xs) = if e /= x then x : (remove e xs) else remove e xs
