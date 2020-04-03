module SplEvaluator where

import Systems.Environment
import Tokens
import Grammar

data Types = SplInt | SplBool | SplSingleList | SplDoubleList
    deriving (Show, Eq)

type Environment = [(Types, String, Exp)]
type State = (Construct, [Construct], Environment)

-- Single-step evaluation for expressions

evalConstruct :: State -> State

evalConstruct ()

-- IfThenElse Evaluation
evalConstruct ((IfThenElse exp c1 c2), rest, e) -- change variable names lol and also create format function
    | evalExp exp e == BoolTrue  = evalConstruct ((head new_cons1), (tail new_cons1), e)
    | otherwise = evalConstruct ((head new_cons2), (tail new_cons2), e) where
        cons1_list = format c1
        cons2_list = format c2
        new_cons1 = cons1_list ++ rest
        new_cons2 = cons2_list ++ rest

-- While Evaluation
evalConstruct ((While exp c), rest, e)
    | evalExp exp e == BoolTrue = 
    | otherwise = where
        cons1_list = format c

-- IntDeclare Evaluation [Choose between default 0 value or null]
evalConstruct ((IntDeclare var), rest, e) = evalConstruct ((head rest), (tail rest), (SplInt, var, Int 0):e)

-- BoolDeclare Evaluation [Choose between default BoolFalse or null]
evalConstruct ((BoolDeclare var), rest, e) = evalConstruct ((head rest), (tail rest), (SplBool, var, BoolFalse):e)

-- VarAssign Evaluation [Might not work] [Split evalconstructs]
evalConstruct ((VarAssign var exp), rest, e)
    | lookUpType var e == SplBool && (exp == BoolTrue || exp == BoolFalse) = evalConstruct ((head rest), (tail rest), update var exp e) 
    | lookUpType var e == SplInt && exp == Int num = evalConstruct ((head rest), (tail rest), update var exp e)
    | lookUpType var e == SplSingleList && exp == List list = evalConstruct ((head rest), (tail rest), update var exp e)
    | lookUpType var e == SplDoubleList && exp == Sequences = evalConstruct ((head rest), (tail rest), update var exp e)
    | otherwise = error "Wrong data type"

-- NewSigleList Evaluation 
evalConstruct ((NewSingleList var), rest, e) = evalConstruct ((head rest), (tail rest), (SplSingleList, var, List "[]"):e)

-- SingleListAssign Push Evaluation
evalConstruct ((SingleListAssign var1 (Push var2 exp)), rest, e) = evalConstruct ((head rest), (tail rest), updateList var1 var2 exp e)

-- SingleListAssign Pop Evaluation
evalConstruct ((SingleListAssign var1 (Pop var2)), rest, e) = evalConstruct

-- DoubleListDeclare Evaluation
evalConstruct ((DoubleListDeclare var exp), rest, e) = evalConstruct ((head rest), (tail rest), (SplDoubleList, var, exp):e)

-- Return Evaluation
evalConstruct ((Return var), rest, e)
    | rest == [] && lookUpType var == SplSingleList = ((Return var), rest, e)
    | otherwise = error "Stupid"


evalExp :: Exp -> Environment -> Exp

evalExp (Int num) e = (Int num)

evalExp (BoolTrue) e = (BoolTrue)

evalExp (BoolFalse) e = (BoolFalse)

-- Variable lookup reduction
evalExp (Var name) e = lookUp name e

evalExp (List list) e = List list

evalExp (Sequences) e = Sequences

-- List length function reduction
evalExp (Length name) e = Int (length (convertToHaskellList (lookUp name e)))

-- List empty function reduction
evalExp (Empty name) e =
    | length (convertToHaskellList (lookUp name e)) == 0 = BoolTrue
    | otherwise = BoolFalse

-- Addition expression reduction
evalExp (Add (Int a) (Int b)) e = Int (a + b)
evalExp (Add e1 e2) e = evalExp (Add (evalExp e1 e) (evalExp e2 e)) e

-- Minus expression reduction
evalExp (Minus (Int a) (Int b)) e = Int (a - b)
evalExp (Minus e1 e2) e = evalExp (Minus (evalExp e1 e) (evalExp e2 e)) e

-- Multiply expression reduction
evalExp (Multiply (Int a) (Int b)) e = Int (a * b)
evalExp (Multiply e1 e2) e = evalExp (Multiply (evalExp e1 e) (evalExp e2 e)) e

-- Divide expression reduction
evalExp (Divide (Int a) (Int b)) e = Int (a / b)
evalExp (Divide e1 e2) e = evalExp (Divide (evalExp e1 e) (evalExp e2 e)) e

-- LessThan expression reduction
evalExp (LessThan (Int a) (Int b)) e = 
    | a < b == True = BoolTrue
    | otherwise = BoolFalse
evalExp (LessThan e1 e2) e = evalExp (LessThan (evalExp e1 e) (evalExp e2 e)) e

-- GreaterThan expression reduction
evalExp (GreaterThan (Int a) (Int b)) e = 
    | a > b == True = BoolTrue
    | otherwise = BoolFalse
evalExp (GreaterThan e1 e2) e = evalExp (GreaterThan (evalExp e1 e) (evalExp e2 e)) e

-- GreaterThanOrEqualTo expression reduction
evalExp (GTE (Int a) (Int b)) e = 
    | a >= b == True = BoolTrue
    | otherwise = BoolFalse
evalExp (GTE e1 e2) e = evalExp (GTE (evalExp e1 e) (evalExp e2 e)) e

-- LessThanOrEqualTo expression reduction
evalExp (LTE (Int a) (Int b)) e = 
    | a <= b == True = BoolTrue
    | otherwise = BoolFalse
evalExp (LTE e1 e2) e = evalExp (LTE (evalExp e1 e) (evalExp e2 e)) e

-- Not expression evaluation
evalExp (Not val) e =
    | val == BoolTrue = BoolFalse
    | val == BoolFalse = BoolTrue
    | otherwise = error "Not a valid boolean expression"
evalExp (Not exp) e = evalExp (Not (evalExp exp e)) e

-- Lookup variable value function
lookUp :: String -> Environment -> Exp
lookUp name env = get3rd (getVarTuple name env)

-- Lookup variable type function
lookUpType :: String -> Environment -> Types
lookUpType name env = get1st (getVarTuple name env)

-- Update variable value within current environment
update :: String -> Exp -> Environment -> Environment
update var exp oldEnv = (lookUpType var oldEnv, var, exp):(delete var oldEnv))

-- Update list function
updateList :: String -> Exp -> Environment -> Environment
updateList var 

-- Delete variable from given environment
delete :: String -> Environment -> Environment
delete _ [] = []
delete var (x:xs)
   | var == get2nd x = [] ++ (delete var xs)
   | var /= get2nd x = [x] ++ (delete var xs) 

-- Variable tuple get function
getVarTuple :: String -> Environment -> (Type, String, Exp)
getVarTuple name (x:xs)
    | name == (get2nd x) = x
    | name /= (get2nd x) = getVarTuple name xs
    | otherwise = error "No variable found"

-- Convert String List to Haskell List Type
convertToHaskellList :: String -> [Int]
convertToHaskellList stringList
    | length haskellList == 0 = [] 
    | otherwise = haskellList where 
    haskellList = convert [x | x <- stringList, x /= '[', x/=']', x/= ',]'

-- Converts strings to integers
convert :: String -> [Int]
convert xs = [(read :: String -> Int) (charToString x) | x <- xs]

-- Converts character to list type
charToString :: Char -> String
charToString c = [c]

get1st (a,_,_) = a

get2nd (_,b,_) = b

get3rd (_,_,c) = c