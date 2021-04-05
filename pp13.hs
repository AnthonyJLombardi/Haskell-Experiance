-- i was not in class on 10/6/20
appm :: Maybe [a] -> Maybe [a] -> Maybe [a]
appm Nothing _ = Nothing
appm _ Nothing = Nothing
appm (Just xs) (Just ys)
  | (length xs + length ys) <= 10 =  Just (xs ++ ys)
  | otherwise = Nothing
