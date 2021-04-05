--I was not in class on 10/13/20
data LongNum = D [Int]

instance Show LongNum where
  show (D []) = ""
  show (D (d:ds)) = show (D ds) ++ show d
