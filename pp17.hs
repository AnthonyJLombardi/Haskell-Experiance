import Text.ParserCombinators.ReadP
import Data.Char

-- I was not in class on 10/20/20
data Sum = Add Int Int
  deriving (Show)
readSum :: ReadP Sum
readSum = do
  fst <- munch1 isDigit
  skipSpaces
  char '+'
  skipSpaces
  snd <- munch1 isDigit
  let first = read fst :: Int
  let second = read snd :: Int
  return $ Add first second
