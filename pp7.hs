{-
I was not in class on 9/10/20
-}

multseq :: [Int] -> [[Int]]
multseq partial@(1000:_) = reverse [partial]
multseq partial@(x:_)
  | x > 1000 = []
  | otherwise = concat [multseq (x*n+1:partial) | n<- [2..x-1]]
