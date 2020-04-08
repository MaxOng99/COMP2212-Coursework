module SplEvaluator where

import System.Environment
import Tokens
import Grammar
import Data.List

data Types = SplInt | SplBool | SplSingleList | SplDoubleList
    deriving (Show, Eq)

type Environment = [(Types, String, Exp)]
type State = (Construct, [Construct], Environment)

evalLoop :: State -> String
evalLoop state = show value where
    ((Return var), rest, env) = evalConstruct state
    (t, name, value) = getVarTuple var env

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


-- IntAssign Evaluation
evalConstruct ((VarAssign var (Int val)), rest, e)
    | lookUpType var e == SplInt = evalConstruct ((head rest), (tail rest), update var (Int val) e)
    | otherwise = error "Cannot assign this variable to an int"

-- BoolAssign Evaluation
evalConstruct ((VarAssign var (BoolTrue)), rest, e)
    | lookUpType var e == SplBool = evalConstruct ((head rest), (tail rest), update var (BoolTrue) e)
    | otherwise = error "Cannot assign this variable to a boolean"

evalConstruct ((VarAssign var (BoolFalse)), rest, e)
    | lookUpType var e == SplBool = evalConstruct ((head rest), (tail rest), update var (BoolFalse) e)
    | otherwise = error "Cannot assign this variable to a boolean"

-- DoubleListAssign Evaluation
evalConstruct ((VarAssign var (Sequences list)), rest, e)
    | lookUpType var e == SplDoubleList = evalConstruct ((head rest), (tail rest), update var (Sequences list) e)
    | otherwise = error "Cannot assign this variable to a Double List"

-- GeneralVarAssign Evaluation
evalConstruct ((VarAssign var exp), rest, e) = evalConstruct ((head rest), (tail rest), update var evaluatedExp e) where
    evaluatedExp = evalExp exp e

-- PushAssign Evaluation
evalConstruct ((StackOperationAssign var1 (Push var2 exp)), rest, e) = case evalExp exp e of
    evaluated1@(Int val) -> evalConstruct ((head rest), (tail rest), updateListPush var1 var2 evaluated1 e)
    evaluated2@(List stringList) -> evalConstruct ((head rest), (tail rest), updateSequencePush var1 var2 evaluated2 e)

-- PopAssign Evaluation
evalConstruct ((StackOperationAssign var1 (Pop var2)), rest, e) = case lookUp var2 e of
    (List _) -> evalConstruct ((head rest), (tail rest), updateListPop var1 var2 e)
    (Sequences _) -> evalConstruct ((head rest), (tail rest), updateSequencePop var1 var2 e)

-- NewSingleList Evaluation 
evalConstruct ((NewSingleList var), rest, e) = evalConstruct ((head rest), (tail rest), (SplSingleList, var, List "[]"):e)

-- DoubleListDeclare Evaluation
evalConstruct ((DoubleListDeclare var exp), rest, e) = evalConstruct ((head rest), (tail rest), (SplDoubleList, var, evaluatedExp):e) where
    evaluatedExp = evalExp exp e

-- Return Evaluation
evalConstruct ((Return var), rest, e)
    | rest == [] && lookUpType var e == SplSingleList = ((Return var), rest, e)
    | otherwise = error "Return type is not of the correct type. It should be of SplSingleList"


evalExp :: Exp -> Environment -> Exp

evalExp (Int num) e = (Int num)

evalExp (BoolTrue) e = (BoolTrue)

evalExp (BoolFalse) e = (BoolFalse)

-- Variable lookup reduction
evalExp (Var name) e = lookUp name e

evalExp (List list) e = List list

evalExp (Sequences list) e = Sequences list

-- List length function reduction
evalExp (Length name) e = Int (length (convertToHaskellList $ lookUp name e))

-- List empty function reduction
evalExp (Empty name) e 
    | length (convertToHaskellList $ lookUp name e) == 0 = BoolTrue
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
evalExp (Divide (Int a) (Int b)) e = Int (a `div` b)
evalExp (Divide e1 e2) e = evalExp (Divide (evalExp e1 e) (evalExp e2 e)) e

-- LessThan expression reduction
evalExp (LessThan (Int a) (Int b)) e 
    | (a < b) == True = BoolTrue
    | otherwise = BoolFalse
evalExp (LessThan e1 e2) e = evalExp (LessThan (evalExp e1 e) (evalExp e2 e)) e

-- GreaterThan expression reduction
evalExp (GreaterThan (Int a) (Int b)) e
    | (a > b) == True = BoolTrue
    | otherwise = BoolFalse
evalExp (GreaterThan e1 e2) e = evalExp (GreaterThan (evalExp e1 e) (evalExp e2 e)) e

-- GreaterThanOrEqualTo expression reduction
evalExp (GTE (Int a) (Int b)) e
    | (a >= b) == True = BoolTrue
    | otherwise = BoolFalse
evalExp (GTE e1 e2) e = evalExp (GTE (evalExp e1 e) (evalExp e2 e)) e

-- LessThanOrEqualTo expression reduction
evalExp (LTE (Int a) (Int b)) e
    | (a <= b) == True = BoolTrue
    | otherwise = BoolFalse
evalExp (LTE e1 e2) e = evalExp (LTE (evalExp e1 e) (evalExp e2 e)) e

-- Not expression evaluation
evalExp (Not BoolTrue) e = BoolFalse
evalExp (Not BoolFalse) e = BoolTrue
evalExp (Not exp) e = evalExp (Not (evalExp exp e)) e

-- Lookup variable value function
lookUp :: String -> Environment -> Exp
lookUp name env = get3rd (getVarTuple name env)

-- Lookup variable type function
lookUpType :: String -> Environment -> Types
lookUpType name env = get1st (getVarTuple name env)

-- Update variable value within current environment
update :: String -> Exp -> Environment -> Environment
update var exp oldEnv = (lookUpType var oldEnv, var, exp):(delete' var oldEnv)

-- Update Environment for Lists when push is called
updateListPush :: String -> String -> Exp -> Environment -> Environment
updateListPush listVar1 listVar2 (Int val) oldEnv
    | (lookUp listVar1 oldEnv) == (lookUp listVar2 oldEnv) = update listVar1 newListExp oldEnv
    | otherwise = union (update listVar1 newListExp oldEnv) (update listVar2 newListExp oldEnv) where
        newListExp = List (show pushedList)
        pushedList = val:(convertToHaskellList $ lookUp listVar2 oldEnv) 
updateListPush _ _ _ _ = error "Attempting to push non-integer value into list"

updateSequencePush :: String -> String -> Exp -> Environment -> Environment
updateSequencePush seqVar1 seqVar2 (List stringList) oldEnv
    | (lookUp seqVar1 oldEnv) == (lookUp seqVar2 oldEnv) = update seqVar1 newSeqExp oldEnv
    | otherwise = union (update seqVar1 newSeqExp oldEnv) (update seqVar1 newSeqExp oldEnv) where
        newSeqExp = Sequences (show pushedSeq) 
        pushedSeq = convertToHaskellList (List stringList):(convertToHaskellList' $ lookUp seqVar2 oldEnv) 
updateSequencePush _ _ _ _ = error "Attempting to push non single list values"

-- Update Environment for Lists when pop is called
updateListPop :: String -> String -> Environment -> Environment
updateListPop intVar listVar oldEnv = union (update intVar intExp oldEnv) (update listVar poppedList oldEnv) where
    intExp = Int (head list)
    poppedList = List (show $ tail list)
    list = case (convertToHaskellList $ lookUp listVar oldEnv) of 
        [] -> error "Cannot pop from an empty list"
        (x:xs) -> (xs)

updateSequencePop :: String -> String -> Environment -> Environment
updateSequencePop listVar seqVar oldEnv = union (update listVar listExp oldEnv) (update seqVar newSeqVar oldEnv) where
    listExp = List (show $ head poppedSeq)
    newSeqVar = Sequences (show $ tail poppedSeq)
    poppedSeq = case (convertToHaskellList' $ lookUp seqVar oldEnv) of
        [] -> error "Cannot pop from an empty sequence"
        (xs:xss) -> (xs:xss)

-- Delete variable from given environment
delete' :: String -> Environment -> Environment
delete' _ [] = []
delete' var (x:xs)
   | var == get2nd x = [] ++ (delete' var xs)
   | var /= get2nd x = [x] ++ (delete' var xs) 

-- Variable tuple get function
getVarTuple :: String -> Environment -> (Types, String, Exp)
getVarTuple _ [] = error "No variable found"
getVarTuple name (x:xs)
    | name == (get2nd x) = x
    | name /= (get2nd x) = getVarTuple name xs
    | otherwise = error "No variable found"

-- Convert single string list into a single haskell list
convertToHaskellList :: Exp -> [Int]
convertToHaskellList (List stringList) = (read stringList) :: [Int]
convertToHaskellList _ = error "Not a list"

-- Convert Sequences into a haskell list of lists
convertToHaskellList' :: Exp -> [[Int]]
convertToHaskellList' (Sequences stringList)
    | length haskellList == 0 = []
    | otherwise = haskellList where
        haskellList = read stringList :: [[Int]]

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

