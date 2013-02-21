--
-- Adapted from the program "infer", believed to have been originally
-- authored by Philip Wadler, and used in the nofib benchmark suite
-- since at least the late 90s.
--

module Term (VarId, Term (..), freeVars) where

import Shows
import qualified Data.Set as Set
import Data.Set (Set)

type  VarId   =  String
data  Term    =  Int Int
              |  Var VarId
              |  Abs VarId Term
              |  App Term Term
              |  Let VarId Term Term

freeVars :: Term -> Set VarId
freeVars t = go t Set.empty
 where
  go (Int _) s = s
  go (Var v) s = Set.insert v s
  go (Abs v t) s = Set.delete v (go t s)
  go (App f t) s = go f (go t s)
  go (Let v a b) s = go a (Set.delete v (go b s))

instance Show Term where
      showsPrec d  =  showsTerm d

showsTerm                     :: Int -> Shows Term
showsTerm d (Int i)           =  shows i
showsTerm d (Var x)           =  showsString x
showsTerm d (Abs x v)         =  showsParenIf (d>0)
                                 (showsString "\\" . showsString x . showsAbs v)
showsTerm d (App t u)         =  showsParenIf (d>1)
                                 (showsTerm 1 t . showsChar ' ' . showsTerm 2 u)
showsTerm d (Let x u v)       =  showsParenIf (d>0)
                                 (showsString "let  "   . showsString x .
                                  showsString " = "     . showsTerm 1 u .
                                  showsString "  in  "  . showsTerm 0 v)
showsAbs                      :: Shows Term
showsAbs (Abs x t)            =  showsString " " . showsString x . showsAbs t
{- ELSE -}
showsAbs t                    =  showsString ". " . showsTerm 0 t
