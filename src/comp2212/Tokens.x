{ 
module Tokens where 
}

%wrapper "posn" 
$digit = 0-9     
$alpha = [a-zA-Z]    
$custom_whitespace = [\ \t\r\f\v]
@newline = [$custom_whitespace]*\n
@singleline_comment = \%\%.*@newline+
@multiline_comment = \%\*[$white ~$white]*\*\%@newline+

tokens :-
  $custom_whitespace+                   ;
  @singleline_comment                   ;  
  @multiline_comment                    ;
  @newline+          { \p -> \s -> TokenNewLine p }
  \.sort      { \p -> \s -> TokenSort p }
  \.length    { \p -> \s -> TokenLength p }
  \.push      { \p -> \s -> TokenPush p }
  \.pop       { \p -> \s -> TokenPop p }
  \.empty     { \p -> \s -> TokenEmpty p }
  if          { \p -> \s -> TokenIf p }
  else        { \p -> \s -> TokenElse p }
  while       { \p -> \s -> TokenWhile p }
  do          { \p -> \s -> TokenDo p }
  Int         { \p -> \s -> TokenTypeInt p }
  Bool        { \p -> \s -> TokenTypeBool p }
  return      { \p -> \s -> TokenReturn p }
  true        { \p -> \s -> TokenTrue p }
  false       { \p -> \s -> TokenFalse p } 
  $digit+     { \p -> \s -> TokenInt (read s) p }
  \[ [$digit \,]* $digit \] { \p -> \s -> TokenSequence s p } 
  \=          { \p -> \s -> TokenEq p }
  \<          { \p -> \s -> TokenLT p }
  \>          { \p -> \s -> TokenGT p }
  \<\=        { \p -> \s -> TokenLTE p }
  \>\=        { \p -> \s -> TokenGTE p }
  \==         { \p -> \s -> TokenEquality p }
  not         { \p -> \s -> TokenNot p }
  \+          { \p -> \s -> TokenPlus p }
  \-          { \p -> \s -> TokenMinus p }
  \*          { \p -> \s -> TokenTimes p }
  \/          { \p -> \s -> TokenDiv p }
  \(          { \p -> \s -> TokenLParen p }
  \)          { \p -> \s -> TokenRParen p }
  \{          { \p -> \s -> TokenLCurly p }
  \}          { \p -> \s -> TokenRCurly p }
  Int\[\]     { \p -> \s -> TokenList p }
  \[\]        { \p -> \s -> TokenEmptyList p }
  Int\[\]\[\] { \p -> \s -> TokenLists p }
  sequences { \p -> \s -> TokenSequences p }
  print     { \p -> \s -> TokenPrint p }
  $alpha [$alpha $digit \_ \’]*   { \p -> \s -> TokenVar s p}

{ 
-- Each action has type :: AlexPosn -> String -> Token 
-- The token type: 
data Token = 
  TokenNewLine AlexPosn        |
  TokenLength AlexPosn         |
  TokenPush AlexPosn           |
  TokenPop AlexPosn            |
  TokenEmpty AlexPosn          |
  TokenIf AlexPosn             |
  TokenElse AlexPosn           | 
  TokenWhile AlexPosn          |
  TokenTypeInt AlexPosn        |
  TokenTypeBool AlexPosn       |
  TokenReturn AlexPosn         |
  TokenTrue AlexPosn           |
  TokenFalse AlexPosn          |
  TokenInt Int AlexPosn        |
  TokenSequence String AlexPosn|
  TokenEq AlexPosn             |
  TokenLT AlexPosn             |
  TokenGT AlexPosn             |
  TokenLTE AlexPosn            |
  TokenGTE AlexPosn            |
  TokenPlus AlexPosn           |
  TokenMinus AlexPosn          |
  TokenTimes AlexPosn          |
  TokenDiv AlexPosn            |
  TokenLParen AlexPosn         |
  TokenRParen AlexPosn         |
  TokenLCurly AlexPosn         |
  TokenRCurly AlexPosn         |
  TokenEmptyList AlexPosn      |
  TokenList AlexPosn           |
  TokenLists AlexPosn          |
  TokenSequences AlexPosn      |
  TokenPrint AlexPosn          |
  TokenDo AlexPosn             |
  TokenVar String AlexPosn     |
  TokenSort AlexPosn           |
  TokenEquality AlexPosn       |
  TokenNot AlexPosn
  deriving (Eq,Show)

tokenPosn :: Token -> String
tokenPosn (TokenPrint (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLength (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPush (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPop (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEmpty (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIf (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenElse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenWhile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeInt  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTypeBool (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenReturn (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTrue  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFalse  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInt _ (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSequence _ (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSequences (AlexPn a l c) ) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEq  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLT  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGT (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLTE (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenGTE (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPlus  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMinus (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenTimes  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDiv (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenList (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLists (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar _ (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEmptyList (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNot (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLCurly (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRCurly (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNewLine (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDo (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSort (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEquality (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
}