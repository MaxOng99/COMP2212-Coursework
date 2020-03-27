module SplEvaluator where

import Systems.Environment
import Tokens
import Grammar

data Types = SplInt | SplBool | SplSingleList | SplDoubleList
    deriving (Show, Eq)

type Environment = [(Types, String, Exp)]
type State = (Construct, [Construct], Environment)

-- Single-step evaluation for expressions
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

-- Variable lookup function
lookUp :: String -> Environment -> Exp
lookUp name env = get3rd (getVarTuple name env)

-- Variable tuple get function
getVarTuple :: String -> Environment -> (Type, String, Exp)
getVarTuple name (x:xs)
    | name == (get2nd x) = x
    | name /= (get2nd x) = getVarTuple name xs
    | otherwise = error "No variable found"

-- Convert String List to Haskell List Type
convertToHaskellList :: String -> [Int]
convertToHaskellList stringList = convert [x | x <- stringList, x /= '[', x/=']', x/= ',']

-- Converts strings to integers
convert :: String -> [Int]
convert xs = [(read :: String -> Int) (charToString x) | x <- xs]

-- Converts character to list type
charToString :: Char -> String
charToString c = [c]

get1st (a,_,_) = a

get2nd (_,b,_) = b

get3rd (_,_,c) = c