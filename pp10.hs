-- i was not in class on 9/22/20
intersect :: Eq a => [a] -> [a] -> [a]
intersect xs ys = filter (`elem` ys) xs

makePos :: (Num a, Ord a) => [a] -> [a]
makePos xs = map (max 0) xs
