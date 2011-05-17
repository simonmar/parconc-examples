module Infer (inferTerm,inferTop) where

import Data.List(nub)

import  MyList                  (minus)
import  Type                  (TVarId, TConId, MonoType (..), PolyType (All),
                               arrow, freeTVarMono)
import  Term
import  Substitution          (Sub, applySub, lookupSub, makeSub)
import  Environment
import  InferMonad
import  MaybeM
import  Control.Monad.Par
import  qualified Data.Set as Set
import  qualified Data.Map as Map
import  Data.Map (Map)
import  Data.Maybe
import Debug.Trace

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

inferTopTerm :: Env -> Term -> MonoType
inferTopTerm aa t = useI (error "type error") (inferTerm aa t)

type TopEnv = Map VarId (IVar PolyType)

inferTop :: TopEnv -> Term -> Par MonoType
inferTop topenv (Let x u v) = do
    vu <- new

    fork $ do
      let fu = Set.toList (freeVars u)
      tfu <- mapM (get . fromJust . flip Map.lookup topenv) fu
      let aa = makeEnv (zip fu tfu)
      put vu (inferTopRhs aa u)

    inferTop (Map.insert x vu topenv) v

inferTop topenv t = do
    let (vs,ivs) = unzip (Map.toList topenv)
    tvs <- mapM get ivs
    let aa = makeEnv (zip vs tvs)
    return (inferTopTerm aa t)
