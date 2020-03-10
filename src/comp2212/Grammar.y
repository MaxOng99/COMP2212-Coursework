{ 
module Grammar where 
import Tokens 
}

%name parseCalc 
%tokentype { Token } 
%error { parseError }
%token
    length { TokenLength _ }
    push   { TokenPush _ }
    pop    { TokenPop _ }
    empty  { TokenEmpty _ }
    if     { TokenIf _ }
    else   { TokenElse _ }
    while  { TokenWhile _ }
    Int    { TokenTypeInt _ }
    Bool   { TokenTypeBool _ }
    return { TokenReturn _ }
    true   { TokenTrue _ }
    false  { TokenFalse _ }
    digit  { TokenInt $$ _ }
    '='    { TokenEq _ }
    '<'    { TokenLT _ }
    '>'    { TokenGT _ }
    '<='   { TokenLTE _ }
    '>='   { TokenGTE _ }
    '+'    { TokenPlus _ }
    '-'    { TokenMinus _ }
    '*'    { TokenTimes _ }
    '/'    { TokenDiv _ }
    '('    { TokenLParen _ } 
    ')'    { TokenRParen _ }
    '[]'   { TokenEmptyList _ }
    'Int[]' { TokenList _ }
    'Int[][]' { TokenLists _ }
    var    { TokenVar $$ _ }
    newline { TokenNewLine _ }
    sequences { TokenSequences _ } 

%nonassoc while
%nonassoc '(' ')'  
%nonassoc if
%nonassoc else
%nonassoc '='
%left '<' '<=' '>=' '>'
%left '+' '-'
%left '*' '/'
%nonassoc length push pop empty
%nonassoc digit true false var 'Int[]' 'Int[][]' sequences '[]' newline

%% 

Construct : if '('Exp')' newline Construct newline else newline Construct    { IfThenElse $3 $6 $10 }
          | while '('Exp')' newline Construct                                  { While $3 $6 }
          | Int var '=' Exp                                                     { IntDeclare $2 $4 }
          | Bool var '=' Exp                                                    { BoolDeclare $2 $4 }
          | 'Int[]' var '=' '[]'                                                { SingleListDeclare $2 }
          | 'Int[][]' var '=' Exp                                               { DoubleListDeclare $2 $4 }
          | var push '('Exp')'                                                  { Push $1 $4 }
          | var pop                                                             { Pop $1 }
          | return var                                                          { Return $2 }
          | Construct newline Construct                                         { Newline $1 $3 }

Exp : digit             { Int $1 }
    | true              { T }
    | false             { F }
    | var               { Var $1 }
    | sequences         { Sequences }
    | var length        { Length $1 }
    | var empty         { Empty $1 }
    | Exp '+' Exp       { Add $1 $3 }
    | Exp '-' Exp       { Minus $1 $3 }
    | Exp '*' Exp       { Multiply $1 $3 }
    | Exp '/' Exp       { Divide $1 $3 }
    | Exp '<' Exp       { LessThan $1 $3 }
    | Exp '>' Exp       { GreaterThan $1 $3 }
    | Exp '>=' Exp      { GTE $1 $3 }
    | Exp '<=' Exp      { LTE $1 $3 }
    | '('Exp')'         { $2 }

{

parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data Construct = IfThenElse Exp Construct Construct
               | While Exp Construct 
               | IntDeclare String Exp 
               | BoolDeclare String Exp
               | SingleListDeclare String
               | DoubleListDeclare String Exp
               | Push String Exp
               | Pop String
               | Return String
               | Newline Construct Construct
               deriving (Show, Eq)

data Exp = Int Int
         | T
         | F
         | Var String
         | Sequences
         | Length String
         | Empty String
         | Add Exp Exp
         | Minus Exp Exp
         | Multiply Exp Exp
         | Divide Exp Exp
         | LessThan Exp Exp
         | GreaterThan Exp Exp
         | GTE Exp Exp
         | LTE Exp Exp
         deriving (Show, Eq)
}
