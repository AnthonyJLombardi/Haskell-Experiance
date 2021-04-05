module TestQ3 (beginA,pigLatin,longest,search,geogDone,geogNext,modify,encryptf,exec',test_exec') where

import Debug.Trace

{-
Solve the first three problems using map, filter or fold.
I solved them using point free style and composition.
-}

{-
beginA takes a list of words and returns a list of
all those words that being with capital A.
-}
beginA :: [String] -> [String]
-- Fill in your code here
beginA = filter (('A'==) . head)

{-
pigLatin takes a list of words and removes the first letter
from each of those words and adds "way" to the end.
-}
pigLatin :: [String] -> [String]
-- Fill in your code here
pigLatin = map ((++ "way") . tail)

{-
longest takes a nonempty list of words and returns the longest
word in that list.  If there is more than one longest
word, it returns the first of them.
-}
longest :: [String] -> String
-- Fill in your code here
longest = foldl1 (\x y -> if length x >= length y then x else y)

{-
Examples:
*TestQ3> fruit = ["Apple","Banana","Apricot","Prune"]
*TestQ3> beginA fruit
["Apple","Apricot"]
*TestQ3> pigLatin fruit
["ppleway","ananaway","pricotway","runeway"]
*TestQ3> longest fruit
"Apricot"
-}


{-
modify takes
  1. an element x of type a
  2. an element y of type b
  3. a function f from a's to b's
it returns a function that is identical to f except it maps x to y
Example:
*TestQ3> notQuiteEven = modify 3 True even
*TestQ3> notQuiteEven 5
False
*TestQ3> notQuiteEven 4
True
*TestQ3> notQuiteEven 3
True
-}
modify :: Eq a => a -> b -> (a -> b) -> (a -> b)
-- Your code goes here
modify x y f = \z -> if z == x then y else f z


{-
encryptf takes
  1. a list xs of a's
  2. a list ys of a's
it returns a function that
  maps elements of xs to the corresponding element of ys
  and maps everything else to itself
Example:
*TestQ3> encrypt = encryptf "abc" "def"
*TestQ3> encrypt 'b'
'e'
*TestQ3> encrypt 'g'
'g'
-}
encryptf :: Eq a => [a] -> [a] -> (a -> a)
-- Your code goes here
encryptf [] _ = \z -> z
encryptf _ [] = \z -> z
encryptf (x:xs) (y:ys) = \z -> (modify x y (encryptf xs ys)) z


{-
exec' is the same as exec in the previous assignment
There is only one difference
  instead of representing memory by pairs (String,Int)
  it represents memory by a function from String to Int
Each variable is mapped to its value
-}

type Inst = (String,String,Int)
type Memf = String -> Int

exec' :: [Inst] -> [Inst] -> Memf -> Int
exec prog cur mem | trace ("exec " ++ show  (head cur) ++ "  " ++ show mem) False = undefined
exec' _ [] mem = 0
exec' prog (("load",var,num):rest) mem
  = exec' prog rest (modify var num mem)
exec' prog (("blz",var,num):rest) mem
  | mem var <= 0 = exec' prog (drop num prog) mem
  | otherwise = exec' prog rest mem
exec' prog (("add",var,num):rest) mem
  = exec' prog rest (modify var (mem var + num) mem)
exec' prog (("jmp",_,num):_) mem
  = exec' prog (drop num prog) mem
exec' prog (("ret",var,_):_) mem
  = mem var

{-
test_exec' calls exec' as in the previous assignment
The only difference is that all variables are should be set to zero
-}
test_exec' :: [Inst] -> Int
test_exec' p = exec' p p (\x -> 0)




{-
Test case for test_exec'

Example:

test_exec' prog1
9

-}

prog1 :: [Inst]
prog1 = [("load","x",4),("load","y",5),("load","z",0),
         ("blz","y",7),("add","z",1),("add","y",(-1)),("jmp","",3),
         ("blz","x",11),("add","z",1),("add","x",(-1)),("jmp","",7),("ret","z",0)]


{-
search takes the following parameters
  1. A function to check if the search is done
    - this function takes:
      a. the global information for the problem
      b. the partial solution
    - the function returns a bool indicating if solution is complete
  2. A function to find the valid next states to go to
    - this function takes:
      a. the global information for the problem
      b. a partial solution
    - the funtion returns a list of valid next states
  3. Global information for the problem
  4. A partial solution
    - a partial solution is a list of states in reverse
search returns a list of all full solutions reachable from
  the partial solution
    - full solutions will be in forward order
-}
search :: (Show a, Show b) =>
  (a -> [b] -> Bool) -> (a -> [b] -> [b]) -> a -> [b] -> [[b]]
--search _ _ _ p | trace ("search  " ++ show p) False = undefined
-- Your code goes here
search isDone nextStates global partial
  | isDone global partial = [reverse partial]
  | otherwise = concat $ map (search isDone nextStates global) $ map (: partial) (nextStates global partial)
{-
The following are used to test search for the maze problem
This is already written, there is nothing for you to do here

Global maze information includes:
  1. Size of the maze
  2. List of blocked cells
  3. End of the maze
-}
type Cell = (Int,Int)
type MazeSolution = [Cell]
type MazeGlobal = (Int,[Cell],Cell)

mazeDone :: MazeGlobal -> MazeSolution -> Bool
mazeDone (_,_,end) (cur:_) = end == cur

mazeNext :: MazeGlobal -> MazeSolution -> [Cell]
mazeNext (size,blocked,_) ((x,y):rest)
  =  filter (`notElem` rest) $
     filter (legalMove size blocked)
       [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

legalMove :: Int -> [Cell] -> Cell -> Bool
legalMove size blocked (x,y)
  = notElem (x,y) blocked && x <= size && y <= size && x >=1 && y >= 1

{-
Test cases for the maze problem

Example:

take 1 $ search mazeDone mazeNext (6,blocks2,(6,6)) [(1,1)]
[[(1,1),(1,2),(2,2),(2,3),(2,4),(1,4),(1,5),(1,6),(2,6),(3,6),(4,6),(4,5),(5,5),(6,5),(6,6)]]

 search mazeDone mazeNext (5,blocks1,(5,5)) [(1,1)]
[]

-}

blocks1 :: [(Int,Int)]
blocks1 = [(1,3),(2,1),(2,5),(3,1),(3,2),(3,3),(3,4),(3,5),(5,6),(2,6)]

blocks2 :: [(Int,Int)]
blocks2 = [(1,3),(2,1),(2,5),(3,1),(3,2),(3,3),(3,4),(3,5),(5,6)]

{-
    ++
    +-
    ++
-----+
-+++-+
++-+++
-}

{-
Write some functions to test search for the geography problem
-}

geogDone :: [String] -> [String] -> Bool
-- Your code goes here
geogDone g p = length g == length p

geogNext :: [String] -> [String] -> [String]
--geogNext g p | trace ("geogNext  parital: " ++ show p ++ " global: "++ show g) False = undefined
-- Your code goes here
geogNext global@(x:xs) partial@(y:_) = filter (`notElem` partial) $ filter (geogLegal y) global


geogLegal :: String -> String -> Bool
geogLegal partial next =  (head $ reverse partial) == head next

{-
Test cases for the geography problem

Example:

search geogDone geogNext wl1 ["ant"]
[["ant","tree","elephant","tiger"]]

search geogDone geogNext wl2 ["ab"]
[["ab","ba","acb","bca","aa"],["ab","ba","aa","acb","bca"],["ab","bca","acb","ba","aa"],["ab","bca","aa","acb","ba"]]

-}

wl1 :: [String]
wl1 = ["ant","tiger","elephant","tree"]

wl2 :: [String]
wl2 = ["ab","ba","acb","bca","aa"]
