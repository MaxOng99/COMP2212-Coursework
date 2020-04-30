module SplEvaluator where

import System.Environment
import Tokens
import Grammar
import Data.List

type Environment = [(String, Exp)]
type State = (Construct, [Construct], Environment)

evalLoop :: State -> IO Exp
evalLoop state = do (((Return var), rest, env)) <- evalConstruct state
                    return (get2nd $ getVarTuple var env)

-- Single-step evaluation for expressions
evalConstruct :: State -> IO State
-- IfThenElse Evaluation
evalConstruct ((IfThenElse exp c1 c2), rest, e)
    | evalExp exp e == BoolTrue = do evalConstruct ((head new_cons1), (tail new_cons1), e)
    | otherwise = do evalConstruct ((head new_cons2), (tail new_cons2), e) where
        cons1_list = formatConstruct c1
        cons2_list = formatConstruct c2
        new_cons1 = cons1_list ++ rest
        new_cons2 = cons2_list ++ rest

evalConstruct ((IfThen exp c), rest, e) 
    | evalExp exp e == BoolTrue = do evalConstruct (head new_cons1, tail new_cons1, e)
    | otherwise = do evalConstruct (head rest, tail rest, e) where
        new_cons1 = cons1_list ++ rest
        cons1_list = formatConstruct c

-- While Evaluation 
evalConstruct ((While exp c), rest, e)
    | evalExp exp e == BoolTrue = do evalConstruct (head new_cons, tail new_cons, e)
    | evalExp exp e == BoolFalse = do evalConstruct (head rest, tail rest, e)
    | otherwise = error "While condition not a boolean" where
        new_cons = cons_list ++ [While exp c] ++ rest
        cons_list = formatConstruct c

-- IntDeclare Evaluation [Choose between default 0 value or null]
evalConstruct ((IntDeclare var), rest, e) = do evalConstruct ((head rest), (tail rest),  (var, Int 0):e)

-- BoolDeclare Evaluation [Choose between default BoolFalse or null]
evalConstruct ((BoolDeclare var), rest, e) = do evalConstruct ((head rest), (tail rest), (var, BoolFalse):e)

-- IntAssign Evaluation
evalConstruct ((VarAssign var (Int val)), rest, e) = do evalConstruct ((head rest), (tail rest), update var (Int val) e)

-- BoolAssign Evaluation
evalConstruct ((VarAssign var (BoolTrue)), rest, e) = do evalConstruct ((head rest), (tail rest), update var (BoolTrue) e)
evalConstruct ((VarAssign var (BoolFalse)), rest, e) = do evalConstruct ((head rest), (tail rest), update var (BoolFalse) e)

-- DoubleListAssign Evaluation
evalConstruct ((VarAssign var (Sequences list)), rest, e) = do evalConstruct ((head rest), (tail rest), update var (Sequences list) e)

-- GeneralVarAssign Evaluation
evalConstruct ((VarAssign var exp), rest, e) = do evalConstruct ((head rest), (tail rest), update var evaluatedExp e) where
    evaluatedExp = evalExp exp e

-- PushAssign Evaluation
evalConstruct ((StackOperationAssign var1 (Push var2 exp)), rest, e) = case evalExp exp e of
    evaluated1@(Int val) -> do evalConstruct ((head rest), (tail rest), updateListPush var1 var2 evaluated1 e)
    evaluated2@(List stringList) -> do evalConstruct ((head rest), (tail rest), updateSequencePush var1 var2 evaluated2 e)

-- PopAssign Evaluation
evalConstruct ((StackOperationAssign var1 (Pop var2)), rest, e) = case lookUp var2 e of
    (List _) -> do evalConstruct ((head rest), (tail rest), updateListPop var1 var2 e)
    (Sequences _) -> do evalConstruct ((head rest), (tail rest), updateSequencePop var1 var2 e)

-- NewSingleList Evaluation 
evalConstruct ((NewSingleList var), rest, e) = do evalConstruct ((head rest), (tail rest), (var, List "[]"):e)

-- DoubleListDeclare Evaluation
evalConstruct ((DoubleListDeclare var Input), rest, e) = do evalConstruct ((head rest), (tail rest), (var, evaluatedExp):e) where
    evaluatedExp = lookUp "splInput" e

-- IntDeclareAssignExp Evaluation
evalConstruct ((IntDeclareAssignExp var exp), rest, e) = do evalConstruct ((head rest), (tail rest), (var, evaluatedExp):e) where
    evaluatedExp = evalExp exp e

-- IntDeclareAssignPop Evaluation
evalConstruct ((IntDeclareAssignPop var list), rest, e) = case lookUp list e of
    (List _) -> do evalConstruct ((head rest), (tail rest), updateListPop var list updatedEnv) where
        var1 = (var, Int 0)
        updatedEnv = var1:e

-- BoolDeclareAssign Evaluation
evalConstruct ((BoolDeclareAssign var exp), rest, e) = do evalConstruct ((head rest), (tail rest), (var, evaluatedExp):e) where
    evaluatedExp = evalExp exp e

-- SingleListDeclareAssignPop Evaluation
evalConstruct ((SingleListDeclareAssignPop var doubleList), rest, e) = case lookUp doubleList e of
    (Sequences _) -> do evalConstruct ((head rest), (tail rest), updateSequencePop var doubleList updatedEnv) where
        list = (var, List "[]")
        updatedEnv = list:e

-- SingleStackOperation Push Evaluation
evalConstruct ((SingleStackOperation (Push var exp)), rest, e) = case evalExp exp e of 
    evaluated1@(Int val) -> do evalConstruct ((head rest), (tail rest), updateListPush var var evaluated1 e)
    evaluated2@(List stringList) -> do evalConstruct ((head rest), (tail rest), updateSequencePush var var evaluated2 e)

-- SingleStackOperation Pop Evaluation
evalConstruct ((SingleStackOperation (Pop var)), rest, e) = case lookUp var e of
    (List _) -> do evalConstruct ((head rest), (tail rest), (update var poppedList e)) where
        poppedList = List (show $ tail list)
        list = case (convertToHaskellList $ lookUp var e) of 
            [] -> error "Cannot pop from an empty list"
            (x:xs) -> (x:xs)
    (Sequences _) -> do evalConstruct ((head rest), (tail rest), (update var poppedSeq e)) where
        poppedSeq = Sequences (show $ tail seq)
        seq = case (convertToHaskellList' $ lookUp var e) of
            [] -> error "Cannot pop from an empty sequence"
            (xs:xss) -> (xs:xss)

evalConstruct ((Sort var), rest, e) = evalConstruct (head rest, tail rest, update var sortedList e) where
    sortedList = List (show $ list)
    list = mergeSort $ convertToHaskellList $ lookUp var e

-- Return Evaluation
evalConstruct ((Return var), rest, e)
    | rest == [] = do return ((Return var), rest, e)
    | otherwise = error "Return must be called at the end of the program"


evalConstruct ((Print exp), rest, e) = do putStrLn (show $ evalExp exp e)
                                          evalConstruct (head rest, tail rest, e)


evalExp :: Exp -> Environment -> Exp

evalExp (Int num) e = (Int num)

evalExp (BoolTrue) e = (BoolTrue)

evalExp (BoolFalse) e = (BoolFalse)

-- Variable lookup reduction
evalExp (Var name) e = lookUp name e

evalExp (List list) e = List list

evalExp (Sequences list) e = Sequences list

evalExp (Input) e = Input


-- List length function reduction
evalExp (Length name) e = case lookUp name e of 
    list@(List xs) -> Int (length $ convertToHaskellList list)
    sequences@(Sequences xss) -> Int (length $ convertToHaskellList' sequences)


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

evalExp (EqualTo (Int a) (Int b)) e
    | (a == b) == True = BoolTrue
    | otherwise = BoolFalse
evalExp (EqualTo e1 e2) e = evalExp (EqualTo (evalExp e1 e) (evalExp e2 e)) e

-- Not expression evaluation
evalExp (Not BoolTrue) e = BoolFalse
evalExp (Not BoolFalse) e = BoolTrue
evalExp (Not exp) e = evalExp (Not (evalExp exp e)) e

-- Lookup variable value function
lookUp :: String -> Environment -> Exp
lookUp name env = get2nd (getVarTuple name env)


-- Update variable value within current environment
update :: String -> Exp -> Environment -> Environment
update var exp oldEnv = (var, exp):(delete' var oldEnv)

-- Update Environment for Lists when push is called
updateListPush :: String -> String -> Exp -> Environment -> Environment
updateListPush listVar1 listVar2 exp oldEnv
    | (lookUp listVar1 oldEnv) == (lookUp listVar2 oldEnv) = update listVar1 newListExp oldEnv
    | otherwise = update listVar2 newListExp env where
        env = update listVar1 newListExp oldEnv
        newListExp = List (show pushedList)
        pushedList = val:(convertToHaskellList $ lookUp listVar2 oldEnv) 
        (Int val) = evalExp exp oldEnv

updateSequencePush :: String -> String -> Exp -> Environment -> Environment
updateSequencePush seqVar1 seqVar2 exp oldEnv
    | (lookUp seqVar1 oldEnv) == (lookUp seqVar2 oldEnv) = update seqVar1 newSeqExp oldEnv
    | otherwise = update seqVar1 newSeqExp env where
        env = update seqVar1 newSeqExp oldEnv
        newSeqExp = Sequences (show pushedSeq) 
        pushedSeq = convertToHaskellList (List stringList):(convertToHaskellList' $ lookUp seqVar2 oldEnv) 
        List stringList = evalExp exp oldEnv

-- Update Environment for Lists when pop is called
updateListPop :: String -> String -> Environment -> Environment
updateListPop intVar listVar oldEnv = (update listVar poppedList env) where
    env = update intVar intExp oldEnv
    intExp = Int (head list)
    poppedList = List (show $ tail list)
    list = case (convertToHaskellList $ lookUp listVar oldEnv) of 
        [] -> error "Cannot pop from an empty list"
        (x:xs) -> (x:xs)

updateSequencePop :: String -> String -> Environment -> Environment
updateSequencePop listVar seqVar oldEnv = update listVar listExp env where
    env = update seqVar newSeqVar oldEnv
    listExp = List (show $ head poppedSeq)
    newSeqVar = Sequences (show $ tail poppedSeq)
    poppedSeq = case (convertToHaskellList' $ lookUp seqVar oldEnv) of
        [] -> error "Cannot pop from an empty sequence"
        (xs:xss) -> (xs:xss)

-- Delete variable from given environment
delete' :: String -> Environment -> Environment
delete' _ [] = []
delete' var (x:xs)
   | var == get1st x = [] ++ (delete' var xs)
   | var /= get1st x = [x] ++ (delete' var xs) 

-- Variable tuple get function
getVarTuple :: String -> Environment -> (String, Exp)
getVarTuple _ [] = error "No variable found"
getVarTuple name (x:xs)
    | name == (get1st x) = x
    | name /= (get1st x) = getVarTuple name xs
    | otherwise = error (name ++ " No variable found")

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
convertToState cons inpList = (head (formatConstruct cons), tail (formatConstruct cons), [("splInput", Sequences (show inpList))])

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort a =
  merge (mergeSort firstFew) (mergeSort lastFew)
    where firstFew = take ((length a) `div` 2) a
          lastFew = drop ((length a) `div` 2) a
-- Expects a and b to already be sorted
merge :: (Ord a) => [a] -> [a] -> [a]
merge a [] = a
merge [] b = b
merge (a:as) (b:bs)
  | a < b     = a:(merge as (b:bs))
  | otherwise = b:(merge (a:as) bs)

get1st (a,_) = a

get2nd (_,b) = b

