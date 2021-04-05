--I was not in class on 10/15/20
firstEven :: [Int] -> Maybe [Int]
firstEven [] = Nothing
firstEven (x:xs)
  | even x = Just xs
  | otherwise = do
    ys <- firstEven xs
    return (x:ys)
