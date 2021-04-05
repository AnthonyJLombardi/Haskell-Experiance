--I was not in class on 10/8/20
data Tree a = Leaf | Node a [Tree a]
  deriving (Show)

t :: Tree Int
t = Node 5 [Node 3 [Node 1 [Node 0 [Leaf,Leaf], Node 2 [Leaf,Leaf]], Node 4 [Leaf,Leaf]], Node 7 [Node 6 [Leaf,Leaf] ,Node 9 [Node 8 [Leaf,Leaf],Node 10 [Leaf,Leaf]] ] ]

vals :: Tree a -> [a]
vals Leaf = []
vals (Node n ts) = [n] ++ (vals $ head ts) ++ (vals $ head $ tail ts)
