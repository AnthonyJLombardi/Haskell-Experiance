--I was not in class on 9/24/20
fixpoint :: (a -> a) -> (a -> Bool) -> a -> a
fixpoint f p x = if p x then x else fixpoint f p (f x)
