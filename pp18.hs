-- I was not in class on 10/22/20
import Text.ParserCombinators.ReadP
import Data.Char

data Tree a = Node a [Tree a]
  deriving Show

tree :: ReadP (Tree Int)
tree = fullTree <++ emptyTree

int :: ReadP Int
int = do
  x <- munch1 isDigit
  let n = read x :: Int
  return n

fullTree :: ReadP (Tree Int)
fullTree = do
  n <- int
  char '('
  xs <- sepByl tree (char ',')
  char ')'
  return $ Node n (xs)


emptyTree :: ReadP (Tree Int)
emptyTree = do
  n <- int
  return $ (Node n [])
