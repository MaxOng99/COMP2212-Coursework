module SplEvaluator where

import Systems.Environment
import Tokens
import Grammar
import Data.List

data Types = SplInt | SplBool | SplSingleList | SplDoubleList
    deriving (Show, Eq)

type Environment = [(Types, String, Exp)]
type State = (Construct, [Construct], Environment)

evalLoop :: State -> String
evalLoop state = value where
    ((Return var), rest, env) = evalConstruct state
    (type, var, value) = getVarTuple var env

-- Single-step evaluation for expressions
evalConstruct :: State -> State
-- IfThenElse Evaluation
evalConstruct ((IfThenElse exp c1 c2), rest, e) -- change variable names lol
    | evalExp exp e == BoolTrue = evalConstruct ((head new_cons1), (tail new_cons1), e)
    | otherwise = evalConstruct ((head new_cons2), (tail new_cons2), e) 
    where
        cons1_list = formatConstruct c1
        cons2_list = formatConstruct c2
        new_cons1 = cons1_list ++ rest
        new_cons2 = cons2_list ++ rest

-- While Evaluation 
evalConstruct ((While exp c), rest, e)
    | evalExp exp e == BoolTrue = evalConstruct (head new_cons, tail new_cons, e)
    | evalExp exp e == BoolFalse = evalConstruct (head rest, tail rest, e) 
    | otherwise = error "While condition not a boolean" where
    	new_cons = cons_list ++ [While exp c] ++ rest
    	cons_list = formatConstruct c

-- IntDeclare Evaluation [Choose between default 0 value or null]
evalConstruct ((IntDeclare var), rest, e) = evalConstruct ((head rest), (tail rest), (SplInt, var, Int 0):e)

-- BoolDeclare Evaluation [Choose between default BoolFalse or null]
evalConstruct ((BoolDeclare var), rest, e) = evalConstruct ((head rest), (tail rest), (SplBool, var, BoolFalse):e)


-- VarAssign Evaluation [Might not work] [Split evalconstructs]
evalConstruct ((VarAssign var exp), rest, e)
    | lookUpType var e == SplBool && (evaluatedExp == BoolTrue || evaluatedExp == BoolFalse) = evalConstruct ((head rest), (tail rest), updatedEnv) 
    | lookUpType var e == SplInt && evaluatedExp == Int num = evalConstruct ((head rest), (tail rest), updatedEnv)
    | lookUpType var e == SplDoubleList && evaluatedExp == Sequences = evalConstruct ((head rest), (tail rest), updatedEnv)
    | otherwise = error "Wrong data type" where 
    	updatedEnv = update var evaluatedExp e
        evaluatedExp = evalExp exp e


-- PushAssign Evaluation
evalConstruct ((StackOperationAssign listVar1 (Push listVar2 exp)), rest, e) = evalConstruct ((head rest), (tail rest), updatedEnv1) where
    updatedEnv1 = updateListPush listVar1 listVar2 evaluatedExp e
    evaluatedExp = evalExp e 

-- PopAssign Evaluation
evalConstruct ((StackOperationAssign intVar (Pop listVar)), rest, e) = evalConstruct ((head rest), (tail rest), updatedEnv2) where
    updatedEnv2 = updateListPop intVar listVar e 

-- NewSingleList Evaluation 
evalConstruct ((NewSingleList var), rest, e) = evalConstruct ((head rest), (tail rest), (SplSingleList, var, List "[]"):e)

-- DoubleListDeclare Evaluation
evalConstruct ((DoubleListDeclare var exp), rest, e) = evalConstruct ((head rest), (tail rest), (SplDoubleList, var, exp):e)

-- Return Evaluation
evalConstruct ((Return var), rest, e)
    | rest == [] && lookUpType var == SplDoubleList = ((Return var), rest, e)
    | otherwise = error "Return type is not of the correct type. It should be of SplDoubleList"


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
evalExp (Empty name) e 
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
evalExp (LessThan (Int a) (Int b)) e 
    | a < b == True = BoolTrue
    | otherwise = BoolFalse
evalExp (LessThan e1 e2) e = evalExp (LessThan (evalExp e1 e) (evalExp e2 e)) e

-- GreaterThan expression reduction
evalExp (GreaterThan (Int a) (Int b)) e
    | a > b == True = BoolTrue
    | otherwise = BoolFalse
evalExp (GreaterThan e1 e2) e = evalExp (GreaterThan (evalExp e1 e) (evalExp e2 e)) e

-- GreaterThanOrEqualTo expression reduction
evalExp (GTE (Int a) (Int b)) e
    | a >= b == True = BoolTrue
    | otherwise = BoolFalse
evalExp (GTE e1 e2) e = evalExp (GTE (evalExp e1 e) (evalExp e2 e)) e

-- LessThanOrEqualTo expression reduction
evalExp (LTE (Int a) (Int b)) e
    | a <= b == True = BoolTrue
    | otherwise = BoolFalse
evalExp (LTE e1 e2) e = evalExp (LTE (evalExp e1 e) (evalExp e2 e)) e

-- Not expression evaluation
evalExp (Not val) e
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
update var exp oldEnv = (lookUpType var oldEnv, var, exp):(delete var oldEnv)

-- Update Environment for Lists when push is called
updateListPush :: String -> String -> Exp -> Environment -> Environment
updateListPush listVar1 listVar2 (Int val) oldEnv =
    | (lookUp listVar1 oldEnv) == (lookUp listVar2 oldEnv) = update listVar1 newListExp oldEnv
    | otherwise = union (update listVar1 newListExp oldEnv) (update listVar2 newListExp oldEnv) where
        newListExp = List (show pushedList)
        pushedList = val:(convertToHaskellList $ lookUp (listVar2 oldEnv))
updateListPush _ _ _ _ = error "Attempting to push non-integer value into list"

-- Update Environment for Lists when pop is called
updateListPop :: String -> String -> Environment -> Environment
updateListPop intVar listVar oldEnv = union (update intVar intExp oldEnv) (update listVar newListExp oldEnv) where
    intExp = x
    newListExp = List (show poppedList)
    poppedList = case (convertToHaskellList $ lookUp (listVar oldEnv)) of 
        [] = error "Cannot pop from an empty list"
        (x:xs) = xs

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

-- Function which splits AST into a list of Constructs
formatConstruct :: Construct -> [Construct]
formatConstruct (Newline cons1 cons2) = formatConstruct cons1 ++ formatConstruct cons2
formatConstruct x = [x]

-- Parses source program and input number sequence and converts it into a state
convertToState :: Construct -> [[Int]] -> State
convertToState cons inpList = (head (formatConstruct cons), tail (formatConstruct cons), [])

get1st (a,_,_) = a

get2nd (_,b,_) = b

get3rd (_,_,c) = c

