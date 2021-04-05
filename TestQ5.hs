module TestQ5 (run,stmt,evalStmt) where
import Debug.Trace
import Data.Char
import Text.ParserCombinators.ReadP

--First 40 min of Nov 3 lecture!!


{-
A statement is one of the following
  1. An if-else statement
     When we read it in, it is of the form:
       if (condition) statement else statement
  2. A while statement
     When we read it in, it is of the form:
       while (condition) statement
  3. An assignment statement
     When we read it in, it is of the form:
       variable = expression;
  4. A block of statements
     When we read it in, it is of the form:
       { statement statement ... statement }
     with zero or more statements in curly brackets
  5. A declaration of a variable
     When we read it in, it is of the form:
       int variable;
     so the only data type is integer
     A variable is initialized as zero when declared
     A variable is made up entirely of letters
-}
data Statement = IfElse Condition Statement Statement |
                 While Condition Statement |
                 Assign Expression Expression |
                 Block [Statement] |
                 Declare Expression
  deriving Show

{-
A condition is read in as one of the following forms:
  1. expression < expression
  2. expression > expression
  3. expression <= expression
  4. expression >= expression
  5. expression == expression
  6. expression != expression
  7. condition && condition
  8. condition || condition
  9. ! condition
Note:  Comparison operators have th highest precedence
  followed by "!", followed by "&&" followed by "||"
-}
data Condition = Less Expression Expression |
                 Greater Expression Expression |
                 LessEq Expression Expression |
                 GreaterEq Expression Expression |
                 Equal Expression Expression |
                 NotEqual Expression Expression |
                 And Condition Condition |
                 Or Condition Condition |
                 Not Condition
  deriving Show

{-
An expression is read in as one of the folowing forms:
  1. expression + expression
  2. expression - expression
  3. expression * expression
  4. expression / expression
  5. variable
  6. number
Note:  "*" and "/" have precedence over "+" and "-"
-}
data Expression = Plus Expression Expression |
                   Minus Expression Expression |
                   Times Expression Expression |
                   Divide Expression Expression |
                   Var String |
                   Num Int
  deriving Show

{-
Memory is a set of pairs consisting of
  - a variable
  - the current value of that variable
Variables could be duplicated in memory
  then I will assume the first occurrence
  of a variable gives the current value
-}
type Memory = [(String,Int)]

{-
This function will parse your input and run the program
A program is a list of statements surrounded by curly brackets
  in other words, a program is a statement
When you run your program, initially the memory is empty
This function will return the memory when the program is completed
-}
run :: String -> Memory
-- fill in your code here
run str = evalStmt (parsews stmt str) []

{-
To evaluate a statement you give
  1. the statement
  2. the current memory
It returns the memory after the statement is executed
-}
evalStmt :: Statement -> Memory -> Memory
--evalStmt stmt mem | trace ("evalStmt \n" ++ show stmt ++ "  " ++ show mem) False = undefined
-- fill in your code here
evalStmt (IfElse cnd st1 st2) mem = if (evalCond cnd mem) then (evalStmt st1 mem) else (evalStmt st2 mem)
evalStmt (While cnd st) mem = if (evalCond cnd mem) then (evalStmt (While cnd st) (evalStmt st mem)) else mem
evalStmt (Assign (Var str) exp2) mem = (str,(evalExp exp2 mem)) : mem
evalStmt (Declare (Var str)) mem = (str,0):mem
evalStmt (Block []) mem = mem
evalStmt (Block (s:sts)) mem = evalStmt (Block sts) (evalStmt s mem)

{-
To evaluate a condition you give
  1. the condition
  2. the current memory
It returns a bool indicating if the condition is true
-}
evalCond :: Condition -> Memory -> Bool
-- fill in your code here
evalCond (Less exp1 exp2) mem = (evalExp exp1 mem) < (evalExp exp2 mem)
evalCond (Greater exp1 exp2) mem = (evalExp exp1 mem) > (evalExp exp2 mem)
evalCond (LessEq exp1 exp2) mem = (evalExp exp1 mem) <= (evalExp exp2 mem)
evalCond (GreaterEq exp1 exp2) mem = (evalExp exp1 mem) >= (evalExp exp2 mem)
evalCond (Equal exp1 exp2) mem = (evalExp exp1 mem) == (evalExp exp2 mem)
evalCond (NotEqual exp1 exp2) mem = (evalExp exp1 mem) /= (evalExp exp2 mem)
evalCond (And cnd1 cnd2) mem = (evalCond cnd1 mem) && (evalCond cnd2 mem)
evalCond (Or cnd1 cnd2) mem = (evalCond cnd1 mem) || (evalCond cnd2 mem)
evalCond (Not cnd) mem = not (evalCond cnd mem)


{-
To evaluate an expression you give
  1. the expression
  2. the current memory
It returns the value of the expression
-}
evalExp :: Expression -> Memory -> Int
-- fill in your code here
evalExp (Plus exp1 exp2) mem = (evalExp exp1 mem) + (evalExp exp2 mem)
evalExp (Minus exp1 exp2) mem = (evalExp exp1 mem) - (evalExp exp2 mem)
evalExp (Times exp1 exp2) mem = (evalExp exp1 mem) * (evalExp exp2 mem)
evalExp (Divide exp1 exp2) mem = (evalExp exp1 mem) `div` (evalExp exp2 mem)
evalExp (Var str) mem = snd $ head $ filter (\(x,y)-> x==str) mem
evalExp (Num n) mem = n

parse :: ReadP a -> String -> a
parse p s
  | null parses          = error "There are no parses"
  | length parses > 2    = error "There is more than one parse"
  | otherwise            = head parses
    where parses = [x | (x,"") <- readP_to_S p s]

parsews :: ReadP a -> String -> a
parsews p s = parse p [c | c <- s, not (isSpace c)]

-- This parses a statement and stores the result
stmt :: ReadP Statement
-- fill in your code here
stmt = block <++ declaration <++assigment <++ ifelse <++ while

ifelse :: ReadP Statement
ifelse = do
  char 'i'
  char 'f'
  cnd <- cond
  stmt1 <- stmt
  char 'e'
  char 'l'
  char 's'
  char 'e'
  stmt2 <- stmt
  return $ IfElse cnd stmt1 stmt2

while :: ReadP Statement
while = do
  char 'w'
  char 'h'
  char 'i'
  char 'l'
  char 'e'
  cnd <- cond
  stmt1 <- stmt
  return $ While cnd stmt1

assigment :: ReadP Statement
assigment = do
  exp1 <- expr
  char '='
  exp2 <- expr
  char ';'
  return $ Assign exp1 exp2

block :: ReadP Statement
block = do
  char '{'
  stm<- many stmt
  char '}'
  return $ Block stm

declaration :: ReadP Statement
declaration = do
  char 'i'
  char 'n'
  char 't'
  exp <- expr
  char ';'
  return $ Declare exp


--chainl1 :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
--This parses a condition and stores the result
cond :: ReadP Condition
-- fill in your code here
cond =  doubleOP <++ singleOP <++ notCond <++ (chainl1 baseCond andOr)

andOr :: ReadP (Condition->Condition->Condition)
andOr = do
  op <- char '&' <++ char '|'
  char '&' <++ char '|'
  return $ makeCondC op

doubleOP :: ReadP Condition
doubleOP = do
  optional $ char '('
  exp1 <- expr
  op1 <- char '>' <++ char '<' <++ char '=' <++ char '!'
  op2 <- char '='
  exp2 <- expr
  optional $ char ')'
  return $ makeCond (op1:[op2]) exp1 exp2

singleOP :: ReadP Condition
singleOP = do
  optional $ char '('
  exp1 <- expr
  op1 <- char '>' <++ char '<'
  exp2 <- expr
  optional $ char ')'
  return $ makeCond [op1] exp1 exp2


notCond :: ReadP Condition
notCond = do
  op <- char '!'
  cnd <- baseCond
  return $ Not cnd

baseCond :: ReadP Condition
baseCond = parenCond +++ cond

parenCond :: ReadP Condition
parenCond = do
  char '('
  cnd <- cond
  char ')'
  return cnd

makeCond :: String -> Expression -> Expression -> Condition
makeCond "<" exp1 exp2 = Less exp1 exp2
makeCond ">" exp1 exp2 = Greater exp1 exp2
makeCond ">=" exp1 exp2 = LessEq exp1 exp2
makeCond "<=" exp1 exp2 = GreaterEq exp1 exp2
makeCond "==" exp1 exp2 = Equal exp1 exp2
makeCond "!=" exp1 exp2 = NotEqual exp1 exp2

makeCondC :: Char -> Condition -> Condition -> Condition
makeCondC '&' cnd1 cnd2 = And cnd1 cnd2
makeCondC '|' cnd1 cnd2 = Or cnd1 cnd2



-- This parses an exprssion and stores the result
expr :: ReadP Expression
-- fill in your code here
expr = chainl1 factor pm

pm :: ReadP (Expression -> Expression -> Expression)
pm = do
  op <- char '+' <++ char '-'
  return $ makeExp op

factor :: ReadP Expression
factor = chainl1 base td

td :: ReadP (Expression -> Expression -> Expression)
td = do
  op <- char '*' <++ char '/'
  return $ makeExp op

base :: ReadP Expression
base = num +++ var +++ parens

parens :: ReadP Expression
parens = do
  char '('
  e <- expr
  char ')'
  return e

num :: ReadP Expression
num = do
  x <- munch1 isDigit
  let n = read x :: Int
  return $ Num n

var :: ReadP Expression
var = do
  s <- munch1 isAlpha
  return $ Var s

makeExp :: Char -> Expression -> Expression -> Expression
makeExp '+' e1 e2 = Plus e1 e2
makeExp '-' e1 e2 = Minus e1 e2
makeExp '*' e1 e2 = Times e1 e2
makeExp '/' e1 e2 = Divide e1 e2
