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
    sort   { TokenSort _ }
    if     { TokenIf _ }
    else   { TokenElse _ }
    while  { TokenWhile _ }
    do     { TokenDo _ }
    Int    { TokenTypeInt _ }
    Bool   { TokenTypeBool _ }
    return { TokenReturn _ }
    true   { TokenTrue _ }
    false  { TokenFalse _ }
    digit  { TokenInt $$ _ }
    string { TokenString $$ _ }
    list   { TokenSequence $$ _ }
    '='    { TokenEq _ }
    '<'    { TokenLT _ }
    '>'    { TokenGT _ }
    '<='   { TokenLTE _ }
    '>='   { TokenGTE _ }
    '=='   { TokenEquality _ }
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
    print    { TokenPrint _ }

%left newline
%nonassoc '<' '<=' '>=' '>' '=='
%left '+' '-'
%left '*' '/'
%nonassoc not var

%% 

Construct : Construct newline Construct                                         { Newline $1 $3 }
          | if '('Exp')' '{' newline Construct newline '}' else '{' newline Construct newline '}' { IfThenElse $3 $7 $13 }
          | if '('Exp')' '{' newline Construct newline '}'                      { IfThen $3 $7 }
          | while '('Exp')' '{' newline Construct newline '}'                                     { While $3 $7 }
          | Int var                                                             { IntDeclare $2 }
          | Bool var                                                            { BoolDeclare $2 }
          | 'Int[]' var '=' '[]'                                                { NewSingleList $2 }
          | 'Int[][]' var '=' Exp                                               { DoubleListDeclare $2 $4 }
          | var '=' Exp                                                         { VarAssign $1 $3 }
          | var '=' StackOperations                                             { StackOperationAssign $1 $3 }
          | Int var '=' Exp                                                     { IntDeclareAssignExp $2 $4 }
          | Int var '=' var pop                                                 { IntDeclareAssignPop $2 $4 }
          | Bool var '=' Exp                                                    { BoolDeclareAssign $2 $4 }
          | 'Int[]' var '=' var pop                                             { SingleListDeclareAssignPop $2 $4 }
          | do StackOperations                                                  { SingleStackOperation $2 }
          | do var sort                                                         { Sort $2 }
          | print '(' Exp ')'                                                   { Print $3 }
          | return var newline                                                  { Return $2 }
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
    | Exp '==' Exp      { EqualTo $1 $3 }
    | not Exp           { Not $2 }
    | var length        { Length $1 }
    | var empty         { Empty $1 }
    | digit             { Int $1 }
    | true              { BoolTrue }
    | false             { BoolFalse }
    | string            { String $1 }
    | var               { Var $1 }
    | list              { List $1 }
    | sequences         { Input }
    | '('Exp')'         { $2 }

{

parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data Construct = IfThenElse Exp Construct Construct
               | IfThen Exp Construct
               | While Exp Construct 
               | IntDeclare String 
               | BoolDeclare String
               | VarAssign String Exp
               | NewSingleList String
               | StackOperationAssign String StackOperations
               | DoubleListDeclare String Exp
               | Return String
               | Newline Construct Construct
               | Print Exp
               | IntDeclareAssignExp String Exp
               | IntDeclareAssignPop String String
               | BoolDeclareAssign String Exp
               | SingleListDeclareAssignPop String String
               | SingleStackOperation StackOperations
               | Sort String
               deriving (Show, Eq)

data StackOperations = Push String Exp
                     | Pop String
                     deriving (Show, Eq)

data Exp = Int Int
         | BoolTrue
         | BoolFalse
         | Var String
         | List String
         | String String
         | Input
         | Sequences String
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
         | EqualTo Exp Exp
         deriving (Show, Eq)

data Types = SplInt | SplBool | SplSingleList | SplDoubleList | SplString
    deriving (Show, Eq)

}
