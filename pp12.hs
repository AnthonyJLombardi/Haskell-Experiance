-- i was not in class on 10/1/20
applyMain :: Int -> (a->a) -> a -> a
applyMain 0 _ x = x
applyMain n f x = f (applyMain (n-1) f x)
