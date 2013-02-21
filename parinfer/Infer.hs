--
-- Adapted from the program "infer", believed to have been originally
-- authored by Philip Wadler, and used in the nofib benchmark suite
-- since at least the late 90s.
--

module Infer (inferTerm,inferTop) where

import Data.List(nub)

import  MyList                  (minus)
import  Type                  (TVarId, MonoType (..), PolyType (All),
                               arrow, intType, freeTVarMono)
import  Term
import  Substitution          (Sub, applySub, lookupSub, makeSub)
import  Environment
import  InferMonad
import  Control.Monad.Par.Scheds.Trace
import  qualified Data.Set as Set
import  qualified Data.Map as Map
import  Data.Map (Map)
import  Data.Maybe
import Control.Monad

specialiseI                   :: PolyType -> Infer MonoType
specialiseI (All xxs tt)      =  freshesI (length xxs) `thenI` (\yys ->
                                 returnI (applySubs xxs yys tt))
applySubs                     :: [TVarId] -> [MonoType] -> MonoType -> MonoType
applySubs xxs yys tt          =  applySub (makeSub (zip xxs yys)) tt
generaliseI                   :: Env -> MonoType -> Infer PolyType
generaliseI aa tt             =  getSubI `thenI` (\s ->
 				 let aaVars = nub (freeTVarSubEnv s aa) in
				 let ttVars = nub (freeTVarMono tt) in
				 let xxs    = ttVars `minus` aaVars in
                                 returnI (All xxs tt)
                                 )
freeTVarSubEnv                :: Sub -> Env -> [TVarId]
freeTVarSubEnv s aa           =  concat (map (freeTVarMono . lookupSub s)
                                             (freeTVarEnv aa))

inferTerm  ::  Env -> Term -> Infer MonoType
inferTerm _  (Int _)  = returnI intType
inferTerm aa (Var x)  =
      (x `elem` domEnv aa)                      `guardI` (
      let ss = lookupEnv aa x in
      specialiseI ss                          `thenI`  (\tt ->
      substituteI tt                          `thenI`  (\uu  ->
                                              returnI  uu)))
inferTerm aa (Abs x v)  =
      freshI                                  `thenI` (\xx ->
      inferTerm (extendLocal aa x xx) v       `thenI` (\vv ->
      substituteI xx                          `thenI` (\uu ->
                                              returnI (uu `arrow` vv))))
inferTerm aa (App t u)  =
      inferTerm aa t                          `thenI` (\tt ->
      inferTerm aa u                          `thenI` (\uu ->
      freshI                                  `thenI` (\xx ->
      unifyI tt (uu `arrow` xx)               `thenI` (\() ->
      substituteI xx                          `thenI` (\vv ->
                                              returnI vv)))))
inferTerm aa (Let x u v)  = do
    ss <- inferRhs aa u
    inferTerm (extendGlobal aa x ss) v

inferRhs :: Env -> Term -> Infer PolyType
inferRhs aa u = do
    uu <- inferTerm aa u
    generaliseI aa uu

inferTopRhs :: Env -> Term -> PolyType
inferTopRhs aa u = useI (error "type error") $ do
    uu <- inferTerm aa u
    generaliseI aa uu

type TopEnv = Map VarId (IVar PolyType)

-- <<inferTop
inferTop :: TopEnv -> [(VarId,Term)] -> Par [(VarId,PolyType)]
inferTop topenv0 binds = do
  topenv1 <- foldM inferBind topenv0 binds                          -- <1>
  mapM (\(v,i) -> do t <- get i; return (v,t)) (Map.toList topenv1) -- <2>
-- >>

-- <<inferBind
inferBind :: TopEnv -> (VarId,Term) -> Par TopEnv
inferBind topenv (x,u) = do
  vu <- new                                                     -- <1>
  fork $ do                                                     -- <2>
    let fu = Set.toList (freeVars u)                            -- <3>
    tfu <- mapM (get . fromJust . flip Map.lookup topenv) fu    -- <4>
    let aa = makeEnv (zip fu tfu)                               -- <5>
    put vu (inferTopRhs aa u)                                   -- <6>
  return (Map.insert x vu topenv)                               -- <7>
-- >>

