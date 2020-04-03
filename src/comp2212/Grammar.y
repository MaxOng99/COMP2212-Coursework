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
    list   { TokenSequence $$ _ }
    '='    { TokenEq _ }
    '<'    { TokenLT _ }
    '>'    { TokenGT _ }
    '<='   { TokenLTE _ }
    '>='   { TokenGTE _ }
    not    { TokenNot _ }
    '+'    { TokenPlus _ }
    '-'    { TokenMinus _ }
    '*'    { TokenTimes _ }
    '/'    { TokenDiv _ }
    '('    { TokenLParen _ } 
    ')'    { TokenRParen _ }
    '{'    { TokenLCurly _ }
    '}'    { TokenRCurly _ }
    '[]'   { TokenEmptyList _ }
    'Int[]' { TokenList _ }
    'Int[][]' { TokenLists _ }
    var    { TokenVar $$ _ }
    newline { TokenNewLine _ }
    sequences { TokenSequences _ } 

%left newline
%nonassoc '<' '<=' '>=' '>'
%left '+' '-'
%left '*' '/'
%nonassoc not

%% 

Construct : Construct newline Construct                                         { Newline $1 $3 }
          | if '('Exp')' '{' newline Construct newline '}' else '{' newline Construct newline '}' { IfThenElse $3 $7 $13 }
          | while '('Exp')' '{' newline Construct newline '}'                                     { While $3 $7 }
          | Int var                                                             { IntDeclare $2 }
          | Bool var                                                            { BoolDeclare $2 }
          | var '=' Exp                                                         { VarAssign $1 $3 }
          | var '=' StackOperations                                             { SingleListAssign $1 $3 }
          | 'Int[]' var '=' '[]'                                                { NewSingleList $2 }
          | 'Int[][]' var '=' Exp                                               { DoubleListDeclare $2 $4 }
          | return var                                                          { Return $2 }

StackOperations : var push '('Exp')'                                            { Push $1 $4 }
                | var pop                                                       { Pop $1 }

Exp : Exp '<' Exp       { LessThan $1 $3 }
    | Exp '>' Exp       { GreaterThan $1 $3 }
    | Exp '>=' Exp      { GTE $1 $3 }
    | Exp '<=' Exp      { LTE $1 $3 }
    | Exp '+' Exp       { Add $1 $3 }
    | Exp '-' Exp       { Minus $1 $3 }
    | Exp '*' Exp       { Multiply $1 $3 }
    | Exp '/' Exp       { Divide $1 $3 }
    | not Exp           { Not $2 }
    | var length        { Length $1 }
    | var empty         { Empty $1 }
    | digit             { Int $1 }
    | true              { BoolTrue }
    | false             { BoolFalse }
    | var               { Var $1 }
    | list              { List $1 }
    | sequences         { Sequences }
    | '('Exp')'         { $2 }

{

parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data Construct = IfThenElse Exp Construct Construct
               | While Exp Construct 
               | IntDeclare String 
               | BoolDeclare String
               | VarAssign String Exp
               | NewSingleList String
               | SingleListAssign String StackOperations
               | DoubleListDeclare String Exp
               | Return String
               | Newline Construct Construct
               deriving (Show, Eq)

data StackOperations = Push String Exp
                     | Pop String
                     deriving (Show, Eq)

data Exp = Int Int
         | BoolTrue
         | BoolFalse
         | Var String
         | List String
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
         | Not Exp
         deriving (Show, Eq)
}
