module TestQ5 (run,stmt,evalStmt) where
import Debug.Trace
import Data.Char
import Text.ParserCombinators.ReadP


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
run fileName = evalStmt (parsews stmt fileName) []

{-
To evaluate a statement you give
  1. the statement
  2. the current memory
It returns the memory after the statement is executed
-}
evalStmt :: Statement -> Memory -> Memory
--evalStmt stmt mem | trace ("evalStmt \n" ++ show stmt ++ "  " ++ show mem) False = undefined
-- fill in your code here
evalStmt (IfElse p s1 s2) mem   = if evalCond p mem then evalStmt s1 mem else evalStmt s2 mem
evalStmt (While p s) mem        = if evalCond p mem then evalStmt (While p s) (evalStmt s mem) else mem
evalStmt (Assign (Var v) e) mem = (v, evalExp e mem):mem
evalStmt (Block (s1:s) ) mem    = evalStmt (Block s) (evalStmt s1 mem)
evalStmt (Block []) mem         = mem
evalStmt (Declare (Var v)) mem  = (v, 0):mem

{-
To evaluate a condition you give
  1. the condition
  2. the current memory
It returns a bool indicating if the condition is true
-}
evalCond :: Condition -> Memory -> Bool
-- fill in your code here
evalCond (Less e1 e2) mem       = evalExp e1 mem < evalExp e2 mem
evalCond (Greater e1 e2) mem    = evalExp e1 mem > evalExp e2 mem
evalCond (LessEq e1 e2) mem     = evalExp e1 mem <= evalExp e2 mem
evalCond (GreaterEq e1 e2) mem  = evalExp e1 mem >= evalExp e2 mem
evalCond (Equal e1 e2) mem      = evalExp e1 mem == evalExp e2 mem
evalCond (NotEqual e1 e2) mem   = evalExp e1 mem /= evalExp e2 mem
evalCond (And s1 s2) mem        = evalCond s1 mem && evalCond s2 mem
evalCond (Or s1 s2) mem         = evalCond s1 mem || evalCond s2 mem
evalCond (Not s) mem            = not(evalCond s mem)


{-
To evaluate an expression you give
  1. the expression
  2. the current memory
It returns the value of the expression
-}
evalExp :: Expression -> Memory -> Int
-- fill in your code here
evalExp (Num n) _ = n
evalExp (Plus e1 e2) mem   = evalExp e1 mem + evalExp e2 mem
evalExp (Minus e1 e2) mem  = evalExp e1 mem - evalExp e2 mem
evalExp (Times e1 e2) mem  = evalExp e1 mem * evalExp e2 mem
evalExp (Divide e1 e2) mem = evalExp e1 mem `div` evalExp e2 mem
evalExp (Var v) mem
  | answer == Nothing = error(v ++ " is not in mem")
  | otherwise         = val
    where answer = lookup v mem
          Just val = answer

-- This parses a statement and stores the result
stmt :: ReadP Statement
-- fill in your code here
stmt = ifElse <++ while <++ assign <++ block <++ declare

ifElse::ReadP Statement
ifElse = do
  string "if"
  char '('
  p <- cond
  char ')'
  s1 <- stmt
  string "else"
  s2 <- stmt
  return (IfElse p s1 s2)

while::ReadP Statement
while = do
  string "while"
  char '('
  p <- cond
  char ')'
  s <- stmt
  return (While p s)

block::ReadP Statement
block = do
  char '{'
  s <- many stmt
  char '}'
  return (Block s)


assign::ReadP Statement
assign = do
  v <- expr
  char '='
  e <- expr
  char ';'
  return (Assign v e)

declare::ReadP Statement
declare = do
  string "int"
  v <- expr
  char ';'
  return (Declare v)


-- This parses a condition and stores the result
cond :: ReadP Condition
-- fill in your code here
cond = chainl1 notOr isOr

isOr::ReadP (Condition -> Condition -> Condition)
isOr = do
  string "||"
  return Or

notOr::ReadP Condition
notOr = chainl1 notAnd isAnd

isAnd::ReadP (Condition -> Condition -> Condition)
isAnd = do
  string "&&"
  return And

notAnd::ReadP Condition
notAnd = notNot <++ isNot

isNot::ReadP Condition
isNot = do
  char '!'
  c <- cond
  return (Not c)

notNot::ReadP Condition
notNot = strictly <++ loosely <++ isEqual

strictly::ReadP Condition
strictly = do
  e1 <- expr
  op <- char '<' <++ char '>'
  e2 <- expr
  if op == '<' then return (Less e1 e2) else return (Greater e1 e2)

loosely::ReadP Condition
loosely = do
  e1 <- expr
  op <- string "<=" <++ string ">="
  e2 <- expr
  if op == "<=" then return (LessEq e1 e2) else return (Greater e1 e2)

isEqual::ReadP Condition
isEqual = do
  e1 <- expr
  op <- string "==" <++ string "/="
  e2 <- expr
  if op == "==" then return (Equal e1 e2) else return (NotEqual e1 e2)


-- This parses an exprssion and stores the result
expr :: ReadP Expression
-- fill in your code here
expr = chainl1 factor plus

plus::ReadP (Expression -> Expression -> Expression)
plus = do
  op <- char '+' <++ char '-'
  if op == '+' then return Plus else return Minus

factor::ReadP Expression
factor = chainl1 base times

times::ReadP (Expression -> Expression -> Expression)
times = do
  op <- char '*' <++ char '/'
  if op == '*' then return Times else return Divide

base::ReadP Expression
base = parens <++ num <++ var

parens::ReadP Expression
parens = do
  char '('
  e <- expr
  char ')'
  return e

num::ReadP Expression
num = do
  x <- munch1 isDigit
  let n = read x::Int
  return (Num n)

var::ReadP Expression
var = do
  s <- munch1 isAlpha
  return (Var s)

-- Helper parse functions
parse :: ReadP a -> String -> a
parse p s    
  | null parses        = error "There are no parses" 
  | length parses > 1  = error "There is more than one parse" 
  | otherwise          = head parses   
    where parses = [x | (x,"") <- readP_to_S p s] 

parsews :: ReadP a -> String -> a 
parsews p s = parse p [c | c <- s, not (isSpace c)] 

