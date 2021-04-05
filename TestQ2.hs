module TestQ2 (prefix,subList,addb,multdig,multlist,addDigits,divisible3,divisible7,threeN,exec,test_exec) where
import Debug.Trace

{-
Prefix takes two lists.
It returns true if the first list is a prefix of the second list

Example:
prefix "abc" "abcdef"
True
prefix "abc" "abc"
True
-}
prefix :: Eq a => [a] -> [a] -> Bool
-- Fill in your code here
prefix [] _ = True
prefix (x:xs) (y:ys)
  | x == y = prefix xs ys
  | otherwise = False


{-
subList takes two lists.
It returns true if the first list is a sublist of the second list.

Example:
subList "bcd" "abcdef"
True
subList "bce" "abcdef"
False
-}
subList :: Eq a => [a] -> [a] -> Bool
-- Fill in your code here
subList _ [] = False
subList xs partial_ys@(_:ys) =
  if xs ==  fst (splitAt (length xs) partial_ys) then True else subList xs ys



{-
Digit is a digit in base 10, in other words an Int in [0..9]
-}
type Digit = Int
{-
Number is a list of digits representing a positive integer
  the list will be in reverse (i.e., from lower order to higher order digit)
  the list will not have leading zeroes
-}
type Number = [Digit]


{-
addb takes 3 arguments
  1. A positive integer, represented as a list of digits, in reverse
       (In other words, it goes from lowest order digit to highest order digit,
        and not containing any leading zeroes)
  2. Another positive integer, represented as a list of digits, in reverse
  3. A carry, which will always be 0 or 1
addb returns the result of adding the two positive integers plus the carry

Example:
addb [5,7] [9,8] 0
[4,6,1]

addb [7,1,1] [6] 0
[3,2,1]

-}
addb :: Number -> Number -> Digit -> Number
-- Fill in your code here
addb [] y c
  | not (null y) = (c+(head y)):(tail y)
  | otherwise = if c == 1 then [c] else []
addb x [] c
  | not (null x) = (c+(head x)):(tail x)
  | otherwise = if c == 1 then [c] else []
addb (x:xs) (y:ys) c
  | (x+y+c) >= 10 = (mod (x+y+c) 10) : (addb xs ys 1)
  | otherwise = (x+y+c) : (addb xs ys 0)


{-
multdig takes two digits and returns the result of multiplying them

Example:
multdig 8 4
[2,3]

-}
multdig :: Digit -> Digit -> Number
-- Fill in your code here
multdig d e = if ans >= 10 then (mod ans 10) : [div ans 10] else [ans]
  where ans = d*e

{-
multlist takes two lists of digits and returns their dot product,
  i.e.,the sum of the products of corresponding elements
Note that a list of digits can also be a number

Example:
multlist [2,3,4] [5,6,7]
[6,5]

Explanation of example: 2*5 + 3*6 + 4*7 = 56

multlist  [1,2,3,4,5,6,7,8,9,3] [1,3,2,6,4,5,1,3,2,6]
[4,5,1]
-}


multlist :: [Digit] -> [Digit] -> Number
-- Fill in your code here
multlist (x:[]) (y:_) = (multdig x y)
multlist (x:_) (y:[]) = (multdig x y)
multlist (x:xs) (y:ys) = addb (multdig x y) (multlist xs ys) 0


{-
addDigits takes a list of digits and returns its sum
Note that a list of digits can also be a number

Example:
addDigits [2,0,5,5,5,5,8]
[0,3]
-}

addDigits ::[Digit] -> Number
-- Fill in your code here
addDigits partial@(x:[]) = partial
addDigits (x:xs) = addb [x] (addDigits xs) 0


{-
divisible3 takes a number and returns a Bool indicating whether it is divisible by 3
Use the following algorithm to solve this:
1. If the number only has one digit, check if that digit is 3,6 or 9
2. Otherwise add the digits and call divisible3 on the sum

Example:
divisible3 [9,9,9]
True
-}

divisible3 :: Number -> Bool
-- Fill in your code here
divisible3 (n:[]) = n==3 || n==6 || n==9
divisible3 n = divisible3 (addDigits n)


{-
divisible7 takes a number and returns a Bool indicating whether it is divisible by 7
Use the following algorithm to solve this:
1. If the number only has one digit, check if that digit is 7
2. Otherwise starting with the lower order digit, multiply the digits successively
  by 1,3,2,6,4,5, and repeat for the length of the number.  Call divisible7 recursively
  on the sum of those products.
Hint:  Use cycle.

Example:
divisible7 [1,2,3,4,5,6,7,8,9,3]
True

Explanation of example:
1*1 + 3*2 + 2*3 + 6*4 + 4*5 + 5*6 + 1*7 + 3*8 + 2*9 + 6*3 = 154
1*4 + 3*5 + 2*1 = 21
1*1 + 3*2 = 7
-}

divisible7 :: Number -> Bool
-- Fill in your code here
divisible7 (n:[]) = n==7
divisible7 ns = divisible7 (multlist ns (cycle [1,3,2,6,4,5]))


{-
threeN takes a list of Integers as argument.
It extends to the beginning of the list in the following way until the first element becomes 1.
1. If the first element is even, it puts n/2 in front
2. If the first element is odd, it puts 3n+1 in front

Example:
threeN [3]
[1,2,4,8,16,5,10,3]
-}
threeN :: [Integer] -> [Integer]
-- Fill in your code here
threeN partial@(1:_) = partial
threeN partial@(n:_)
  | (mod n 2) == 0 =  threeN ((div n 2) : partial)
  | otherwise =  threeN ((3*n+1) : partial)


{-
Statement is an assembly language statement with three parts
  1. an instruction i
  2. a variable name v
  3. an integer n
Program is a list of Statements, to be executed in order
  implicitly think of each statement in the program to have a line number
  with the first instruction at line number 0
Memory represents the memory of the computer
  Memory is a list of pairs of a variable and its assigned value
The meaning of each instruction is as follows:
  1. load v n
    give variable v the value n in memory
  2. add v n
    add n to the value of v in memory
  3. jmp v n
    go to line number n, note that v is ignored so anything is allowed
  4. blz v n
    if the value of v is <= 0 then go line n
      otherwise proceed to the next line in the program
  5. ret v n
    quit the program and return the value v, here n is ignored
-}
type Inst = String
type Variable = String
type Statement = (Inst,Variable,Int)
type Program = [Statement]
type Memory = [(Variable,Int)]


{-
exec executes your program (or a part of your program)
  and returns its return value
A program is executed by executing each instruction in order, except for jmp or blz
exec takes as parameters:
  1. The entire program
  2. The piece of the program that is currently being evaluated
    (i.e., the current instruction up to the end of the program)
It returns the result of the first return statement it encounters
Note 1: The simplest way to update a value is to add a new pair to memorey
  instead of changing the value of what is there
Note 2: I don't care what you do if there are errors, such as:
  1. syntax errors
  2. jumping out of the program
  3. not encountering a return statement
Your function will actually be an interpreter
  - It will take a partial program and a list of memory values and execute the first line of the program
  - Then call itself recursively on a new program and a new list of memory values
This problem is much easier if you use pattern matching

 exec prog1 prog1 []
9

-}
exec :: Program -> Program -> Memory -> Int
-- Uncomment the following line if you want to trace your code
-- exec prog cur mem | trace ("exec " ++ show  (head cur) ++ "  " ++ show mem) False = undefined
-- Fill in your code here
exec _ prog_partial@(("ret",var,_):_) mem = (getValMem mem var) --ends the program
exec prog_whole prog_partial@((inst,var,n):_) mem
  | inst == "load" = exec prog_whole (tail prog_partial) (mem ++ [(var,n)]) --loads variable into memory then goes to the next instruction
  | inst == "add" = exec prog_whole (tail prog_partial) (addValMem mem var n) --addes n to var in mem
  | inst == "jmp" = exec prog_whole (drop n prog_whole) mem --jumps to line n
  | inst == "blz" = if (getValMem mem var) <= 0
    then exec prog_whole (drop n prog_whole) mem else exec prog_whole (tail prog_partial) mem --jumpes to line n if the value of var is <=0
  | otherwise = exec prog_whole (tail prog_partial) mem --goes to next instruction in program

--returns a integer value for a given variable in memory
getValMem :: Memory -> Variable -> Int
getValMem [] var = error ("Cannot Locate " ++ var ++ " in memory")
getValMem ((v,m):mem_partial) var = if v == var then m else getValMem mem_partial var

--adds n to var in mem
addValMem :: Memory -> Variable -> Int -> Memory
addValMem [] var _ = error ("Cannot Locate " ++ var ++ " in memory")
addValMem ((v,m):mem_partial) var n = if v == var then (v,m+n) : mem_partial
  else (v,m) : addValMem mem_partial var n

{-
Initially calling exec function

test_exec prog1
9

-}
test_exec :: Program -> Int
--No need to fill in code here
test_exec p = exec p p []


{-
Example program
-}
prog1 :: Program
prog1 = [("load","x",4),("load","y",5),("load","z",0),
         ("blz","y",7),("add","z",1),("add","y",(-1)),("jmp","",3),
         ("blz","x",11),("add","z",1),("add","x",(-1)),("jmp","",7),("ret","z",0)]





{-
geography takes as arguments two lists of words, A and B.
It returns a list of all words C with the following properties:
1. B is a suffix of C. B is the last in C
2. Every word in C, except the first, must have the same last letter
     as the first letter of the word before it.
3. C contains all the words of A but no other words.
You can assume the following:
1. B has property 2.
2. All the words in B are in A.
3. A has no duplicate words.
4. B has no duplicate words.

Note: append words from A to the front of B and call recursively until A is empty

Example:
geography ["tiger","elephant","spot","rat","turtle"] ["spot"]
[["elephant","turtle","rat","tiger","spot"],["rat","tiger","elephant","turtle","spot"]]
-}
geography :: [String] -> [String] -> [[String]]
-- Uncomment the following line if you want to trace your code
-- geography e listSoFar | trace ("geography " ++ show  listSoFar++" Word List: "++show e) False = undefined
-- Fill in your code here
geography [] listSoFar = [listSoFar]
geography parital_wordList@(a:as) parital@(b:_)
  | head (reverse parital) == a = geography as parital
  | head (reverse b) == head a = geography as (a : parital)
  | otherwise = geography (as ++ [a]) parital
