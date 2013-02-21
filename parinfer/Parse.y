-- An example demonstrating how to connect a Happy parser to an Alex lexer.
{
module Parse where
import Lex
import Term
}

%name parseExp Exp
%name parseBinds binds
%tokentype { Token }
%monad { Either String }

%token  let             { TLet     }
        in              { TIn      }
        int             { TInt $$  }
        var             { TVar $$  }
        '->'            { TArrow   }
        '='             { TSym '=' }
        '+'             { TSym '+' }
        '-'             { TSym '-' }
        '*'             { TSym '*' }
        '/'             { TSym '/' }
        '('             { TSym '(' }
        ')'             { TSym ')' }
        '\\'            { TSym '\\' }
        ';'             { TSym ';' }

%%

binds :: { [(VarId,Term)] }
binds : {- empty -}             { [] }
      | bind binds              { $1 : $2 }

bind :: { (VarId,Term) }
bind : var '=' Exp ';'          { ($1,$3) }

Exp :: { Term }
Exp : let var '=' Exp in Exp    { Let $2 $4 $6 }
    | '\\' var '->' Exp         { Abs $2 $4 }
    | Exp1                      { $1 }

Exp1 : Exp1 '+' Term            { App (App (Var "+") $1) $3 }
     | Exp1 '-' Term            { App (App (Var "-") $1) $3 }
     | Term                     { $1 }

Term : Term '*' App            { App (App (Var "*") $1) $3 }
     | Term '/' App            { App (App (Var "/") $1) $3 }
     | App                     { $1 }

App  : App Atom                 { App $1 $2 }
     | Atom                     { $1 }

Atom : int                      { Int $1 }
     | var                      { Var $1 }
     | '(' Exp ')'              { $2 }

{
happyError :: [Token] -> Either String a
happyError [] = Left "Parse error at end of input"
happyError (tk:tks) = Left ("Parse error before " ++ show tk)
}
