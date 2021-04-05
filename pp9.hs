-- I was not in class on 9/17/20
onlyEven ::  (Eq a, Integral a) => [a] -> [a]
onlyEven [] = []
onlyEven (n:ns) = if (mod n 2) == 0 then n : onlyEven ns else onlyEven ns
