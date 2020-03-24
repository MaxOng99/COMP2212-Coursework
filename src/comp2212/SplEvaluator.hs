module SplEvaluator where

import Systems.Environment
import Tokens
import Grammar

data Types = SplInt | SplBool | SplSingleList | SplDoubleList
    deriving (Show, Eq)

type Environment = [(Types, String, Exp)]
type State = (Construct, [Construct], Environment)

evalExp :: Exp -> Environment -> Exp
evalExp (Int num) e = (Int num)

evalExp (BoolTrue) e = (BoolTrue)

evalExp (BoolFalse) e = (BoolFalse)

evalExp (Var name) e = lookUp name e

evalExp (List list) e = --Make List into Haskell list type noob cheng

evalExp (Sequences) e = Sequences

evalExp (Length name) e = Int (length (evalExp (lookUp name e)))

evalExp (Empty name) e =
    | length (evalExp (lookUp name e)) == 0 = BoolTrue
    | otherwise = BoolFalse

evalExp (Add (Int a) (Int b)) e = Int (a + b)
evalExp (Add e1 e2) e = evalExp (Add (evalExp e1 e) (evalExp e2 e)) e

evalExp (Minus (Int a) (Int b)) e = Int (a - b)
evalExp (Minus e1 e2) e = evalExp (Minus (evalExp e1 e) (evalExp e2 e)) e

evalExp (Multiply (Int a) (Int b)) e = Int (a * b)
evalExp (Multiply e1 e2) e = evalExp (Multiply (evalExp e1 e) (evalExp e2 e)) e

evalExp (Divide (Int a) (Int b)) e = Int (a / b)
evalExp (Divide e1 e2) e = evalExp (Divide (evalExp e1 e) (evalExp e2 e)) e

evalExp (LessThan (Int a) (Int b)) e = 
    | a < b == True = BoolTrue
    | otherwise = BoolFalse
evalExp (LessThan e1 e2) e = evalExp (LessThan (evalExp e1 e) (evalExp e2 e)) e

evalExp (GreaterThan (Int a) (Int b)) e = 
    | a > b == True = BoolTrue
    | otherwise = BoolFalse
evalExp (GreaterThan e1 e2) e = evalExp (GreaterThan (evalExp e1 e) (evalExp e2 e)) e

evalExp (GTE (Int a) (Int b)) e = 
    | a >= b == True = BoolTrue
    | otherwise = BoolFalse
evalExp (GTE e1 e2) e = evalExp (GTE (evalExp e1 e) (evalExp e2 e)) e

evalExp (LTE (Int a) (Int b)) e = 
    | a <= b == True = BoolTrue
    | otherwise = BoolFalse
evalExp (LTE e1 e2) e = evalExp (LTE (evalExp e1 e) (evalExp e2 e)) e

evalExp (Not val) e =
    | val == BoolTrue = BoolFalse
    | val == BoolFalse = BoolTrue
    | otherwise = error "Not a valid boolean expression"
evalExp (Not exp) e = evalExp (Not (evalExp exp e)) e

lookUp :: String -> Environment -> Exp
lookUp name env = get3rd (getVarTuple name env)

getVarTuple :: String -> Environment -> (Type, String, Exp)
getVarTuple name (x:xs)
    | name == (get2nd x) = x
    | name /= (get2nd x) = getVarTuple name xs
    | otherwise = error "No variable found"

get1st (a,_,_) = a

get2nd (_,b,_) = b

get3rd (_,_,c) = c