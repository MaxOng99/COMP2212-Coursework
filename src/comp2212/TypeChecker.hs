module TypeChecker where 
import Grammar
import Data.Maybe

type TypeEnvironment = [ (String,Types) ]
type TypeCheckerState = (Construct, [Construct], TypeEnvironment) 

initiateTypeChecking :: Construct -> Bool
initiateTypeChecking c = isCorrect $ typeOfConstruct (c1, rest, tenv) where
    c1 = fromJust $ safeHead cons_list
    rest = safeTail cons_list
    cons_list = formatConstruct c
    tenv = []

-- Type checker for constructs -- 
typeOfConstruct :: TypeCheckerState -> TypeCheckerState
typeOfConstruct (c, [], tenv) = (c, [], tenv) 
typeOfConstruct (IfThenElse exp c1 c2, rest, tenv)
    | typeOfExp exp tenv == SplBool = typeOfConstruct (fromJust $ safeHead new_rest, safeTail new_rest, tenv) 
    | otherwise = error ("if-then-else construct expects a boolean expression. '" ++ show exp ++ "' is not a boolean") where
        new_rest = formatConstruct c1 ++ formatConstruct c2 ++ rest

typeOfConstruct (IfThen exp c, rest, tenv)
    | typeOfExp exp tenv == SplBool = typeOfConstruct (fromJust $ safeHead new_rest, safeTail new_rest, tenv)
    | otherwise = error ("if construct expects a boolean expression. '" ++ show exp ++ "' is not a boolean") where
        new_rest = formatConstruct c ++ rest

typeOfConstruct (While exp c, rest, tenv)
    | typeOfExp exp tenv == SplBool = typeOfConstruct (fromJust $ safeHead new_rest, safeTail new_rest, tenv)
    | otherwise = error ("While construct expects a boolean expression. '" ++ (show exp) ++ "' is not a boolean") where
        new_rest = formatConstruct c ++ rest

typeOfConstruct (IntDeclare var, rest, tenv) = typeOfConstruct (fromJust $ safeHead rest, safeTail rest, addBinding var SplInt tenv)

typeOfConstruct (BoolDeclare var, rest, tenv) = typeOfConstruct (fromJust $ safeHead rest, safeTail rest, addBinding var SplBool tenv)

typeOfConstruct (VarAssign var exp, rest, tenv)
    | varType == expType = typeOfConstruct(fromJust $ safeHead rest, safeTail rest, tenv)
    | otherwise = error (var ++ " expects type " ++ show varType ++ ", but" ++ show expType ++ " was supplied to it") where
        varType = lookUpType var tenv
        expType = typeOfExp exp tenv

typeOfConstruct (StackOperationAssign var1 (Push var2 exp), rest, tenv)
    | var1Type == SplSingleList = if var2Type == SplSingleList && expType == SplInt then newState else error (var1 ++ " expects an integer")
    | var1Type == SplDoubleList = if var2Type == SplDoubleList && expType == SplSingleList then newState else error (var1 ++ " expects a single list") where
        newState = typeOfConstruct (fromJust $ safeHead rest, safeTail rest, tenv)
        expType = typeOfExp exp tenv
        var1Type = lookUpType var1 tenv
        var2Type = lookUpType var2 tenv

typeOfConstruct (StackOperationAssign var1 (Pop var2), rest, tenv)
    | lookUpType var1 tenv == SplInt = if lookUpType var2 tenv == SplSingleList then newState else error (var1 ++ " expects an integer.")
    | lookUpType var1 tenv == SplSingleList = if lookUpType var2 tenv == SplDoubleList then newState else error (var1 ++ " expects a single list")
        where newState = typeOfConstruct (fromJust $ safeHead rest, safeTail rest, tenv)

typeOfConstruct (NewSingleList var, rest, tenv) = typeOfConstruct (fromJust $ safeHead rest, safeTail rest, addBinding var SplSingleList tenv)

typeOfConstruct (DoubleListDeclare var exp, rest, tenv) = typeOfConstruct (fromJust $ safeHead rest, safeTail rest, addBinding var SplDoubleList tenv)

typeOfConstruct (Print exp, rest, tenv) = typeOfConstruct (fromJust $ safeHead rest, safeTail rest, tenv)
typeOfConstruct (Return var, rest, tenv) = (Return var, [], tenv)

typeOfConstruct (IntDeclareAssignExp var exp, rest, tenv)
    | expType == SplInt = typeOfConstruct (fromJust $ safeHead rest, safeTail rest, addBinding var SplInt tenv)
    | otherwise = error ("'" ++ var ++ "' expects type SplInt, but " ++ show expType ++ " was supplied to it") where
        expType = typeOfExp exp tenv

typeOfConstruct (IntDeclareAssignPop var list, rest, tenv)
    | listType == SplSingleList = typeOfConstruct (fromJust $ safeHead rest, safeTail rest, addBinding var SplInt tenv)
    | otherwise = error (var ++ " expects type SplInt, but " ++ show listType ++ " was supplied to it") where
        listType = lookUpType list tenv

typeOfConstruct (BoolDeclareAssign var exp, rest, tenv)
    | expType == SplBool = typeOfConstruct (fromJust $ safeHead rest, safeTail rest, addBinding var SplBool tenv)
    | otherwise = error ("'" ++ var ++ "' expects type SplBool, but " ++ show expType ++ " was supplied to it") where
        expType = typeOfExp exp tenv

typeOfConstruct (SingleListDeclareAssignPop var doubleList, rest, tenv)
    | expType == SplDoubleList = typeOfConstruct (fromJust $ safeHead rest, safeTail rest, addBinding var SplSingleList tenv)
    | otherwise = error (var ++ " expects type SplSingleList, but " ++ show expType ++ " was supplied to it") where
        expType = lookUpType doubleList tenv

typeOfConstruct (SingleStackOperation (Push var exp), rest, tenv)
    | lookUpType var tenv == SplSingleList = if expType == SplInt then newState else error ("Cannot push element of type " ++ show expType ++ " to SplSingleList")
    | lookUpType var tenv == SplDoubleList = if expType == SplSingleList then newState else error ("Cannot push element of type " ++ show expType ++ " to SplDoubleList")
    | otherwise = error (var ++ " is not of a list type. Push operation must be done on a list type") where
        newState = typeOfConstruct (fromJust $ safeHead rest, safeTail rest, tenv) 
        expType = typeOfExp exp tenv

typeOfConstruct (SingleStackOperation (Pop var), rest, tenv)
    | lookUpType var tenv == SplSingleList || lookUpType var tenv == SplDoubleList = typeOfConstruct (fromJust $ safeHead rest, safeTail rest, tenv) 
    | otherwise = error ("Unable to pop from " ++ show var ++ ". Pop operation not supported for " ++ (show $ lookUpType var tenv))

typeOfConstruct (Sort var, rest, tenv)
    | lookUpType var tenv == SplSingleList = typeOfConstruct (fromJust $ safeHead rest, safeTail rest, tenv)
    | otherwise = error (show var ++ " is not of type SplSingleList. Sort operation not supported for " ++ (show $ lookUpType var tenv))

-- Type checker for expressions -- 
typeOfExp :: Exp -> TypeEnvironment -> Types

typeOfExp (Int _) tenv = SplInt
typeOfExp (BoolTrue) tenv = SplBool
typeOfExp (BoolFalse) tenv = SplBool
typeOfExp (Var x) tenv = lookUpType x tenv

typeOfExp (List _) tenv =  SplSingleList
typeOfExp (Sequences _) tenv = SplDoubleList

typeOfExp (Length var) tenv
    | lookUpType var tenv == SplSingleList || lookUpType var tenv == SplDoubleList = SplInt
    | otherwise = error ("length must be called on a list type. " ++ var ++ " is not a list type")

typeOfExp (Empty var) tenv
    | lookUpType var tenv == SplSingleList || lookUpType var tenv == SplDoubleList = SplBool
    | otherwise = error ("empty must be called on a list type. " ++ var ++ " is not a list type")

typeOfExp (Add e1 e2) tenv = checkArithmetic "+" e1 e2 tenv
typeOfExp (Minus e1 e2) tenv = checkArithmetic "-" e1 e2 tenv
typeOfExp (Multiply e1 e2) tenv = checkArithmetic "*" e1 e2 tenv
typeOfExp (Divide e1 e2) tenv = checkArithmetic "/" e1 e2 tenv

typeOfExp (LessThan e1 e2) tenv = checkComparator "<" e1 e2 tenv
typeOfExp (GreaterThan e1 e2) tenv = checkComparator ">" e1 e2 tenv
typeOfExp (LTE e1 e2) tenv = checkComparator "<=" e1 e2 tenv
typeOfExp (GTE e1 e2) tenv = checkComparator ">=" e1 e2 tenv
typeOfExp (Not e) tenv
    | typeOfExp e tenv == SplBool = SplBool
    | otherwise = error "not expects a boolean. Argument provided is not a boolean"



-- Helper Functions -- 
isCorrect :: TypeCheckerState -> Bool
isCorrect (c1, [], tenv) = True
isCorrect (c1, (x:xs), tenv) = False

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

checkArithmetic :: String -> Exp -> Exp -> TypeEnvironment -> Types
checkArithmetic operator e1 e2 tenv
    | typeOfExp e1 tenv == SplInt && typeOfExp e2 tenv == SplInt = SplInt
    | typeOfExp e1 tenv /= SplInt = error (operator ++ " expects two integers. '" ++ (show e1) ++ "' is not an interger")
    | typeOfExp e2 tenv /= SplInt = error (operator ++ " expects two integers. '" ++ (show e2) ++ "' is not an integer")
    | otherwise = error (operator ++ " expects two integers. '" ++ (show e1) ++ "' and '" ++ (show e2) ++ "' are not integers")

checkComparator :: String -> Exp -> Exp -> TypeEnvironment -> Types
checkComparator operator e1 e2 tenv
    | typeOfExp e1 tenv == SplInt && typeOfExp e2 tenv == SplInt = SplBool
    | typeOfExp e1 tenv /= SplInt = error (operator ++ " expects two boolean. First argument is not a boolean")
    | typeOfExp e2 tenv /= SplInt = error (operator ++ " expects two boolean. Second argument is not a boolean")
    | otherwise = error (operator ++ " expects two boolean. Bothh arguemtns are not boolean")

lookUpType :: String -> TypeEnvironment -> Types
lookUpType x [] = error ("Variable '" ++ x ++ "' not declared")
lookUpType x ((var, t):tenv) | x == var = t
                             | otherwise = lookUpType x tenv

addBinding :: String -> Types -> TypeEnvironment -> TypeEnvironment
addBinding var varType tenv = (var, varType):tenv

formatConstruct :: Construct -> [Construct]
formatConstruct (Newline cons1 cons2) = formatConstruct cons1 ++ formatConstruct cons2
formatConstruct x = [x]