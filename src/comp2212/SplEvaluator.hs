module SplEvaluator where

import Systems.Environment
import Tokens
import Grammar

data Types = SplInt | SplBool | SplSingleList | SplDoubleList
    deriving (Show, Eq)

type Environment = [(Types, String, Exp)]
type State = (Construct, [Construct], Environment)


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

evalExp (Var name) e = lookUp name e

evalExp (List list) e = List list

evalExp (Sequences) e = Sequences

evalExp (Length name) e = Int (length (convertToHaskellList (lookUp name e)))

evalExp (Empty name) e =
    | length (convertToHaskellList (lookUp name e)) == 0 = BoolTrue
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

-- change to lookUpValue
lookUp :: String -> Environment -> Exp
lookUp name env = get3rd (getVarTuple name env)

lookUpType :: String -> Environment -> Types
lookUpType name env = get1st (getVarTuple name env)

update :: String -> Exp -> Environment -> Environment
update var exp oldEnv = (lookUpType var oldEnv, var, exp):(delete var oldEnv))

updateList :: String -> Exp -> Environment -> Environment
updateList var 

delete :: String -> Environment -> Environment
delete _ [] = []
delete var (x:xs)
   | var == get2nd x = [] ++ (delete var xs)
   | var /= get2nd x = [x] ++ (delete var xs) 


getVarTuple :: String -> Environment -> (Type, String, Exp)
getVarTuple name (x:xs)
    | name == (get2nd x) = x
    | name /= (get2nd x) = getVarTuple name xs
    | otherwise = error "No variable found"


convertToHaskellList :: String -> [Int]
convertToHaskellList stringList
    | length haskellList == 0 = [] 
    | otherwise = haskellList where 
    haskellList = convert [x | x <- stringList, x /= '[', x/=']', x/= ',]'

convert :: String -> [Int]
convert xs = [(read :: String -> Int) (charToString x) | x <- xs]

charToString :: Char -> String
charToString c = [c]

get1st (a,_,_) = a

get2nd (_,b,_) = b

get3rd (_,_,c) = c