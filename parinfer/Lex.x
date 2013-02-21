{
module Lex where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+                               ;
  "--".*                                ;
  let                                   { \s -> TLet }
  in                                    { \s -> TIn }
  $digit+                               { \s -> TInt (read s) }
  "->"                                  { \s -> TArrow }
  [\; \\ \=\+\-\*\/\(\)]                  { \s -> TSym (head s) }
  $alpha [$alpha $digit \_ \']*         { \s -> TVar s }

{
-- Each right-hand side has type :: String -> Token

-- The token type:
data Token =
        TLet            |
        TIn             |
        TSym Char       |
        TVar String     |
        TInt Int        |
        TArrow
        deriving (Eq,Show)
}
