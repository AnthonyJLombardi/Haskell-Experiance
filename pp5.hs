{-
  I was in class on 9/4/20
-}
maxBeg :: Num a => [a] -> a
maxBeg (x:y:xs) = x+y
maxBeg (x:xs) = x
