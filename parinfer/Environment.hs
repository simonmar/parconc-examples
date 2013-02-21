--
-- Adapted from the program "infer", believed to have been originally
-- authored by Philip Wadler, and used in the nofib benchmark suite
-- since at least the late 90s.
--

module Environment
       (Env, emptyEnv, extendLocal, extendGlobal,
        makeEnv, unmakeEnv, lookupEnv, domEnv, freeTVarEnv)
       where

import Shows
import Term           (VarId)
import Type
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map

newtype  Env          =   MkEnv (Map VarId PolyType)
rep                   ::  Env -> Map VarId PolyType
rep (MkEnv f)         =   f
emptyEnv              ::  Env
emptyEnv              =   MkEnv Map.empty
extendLocal           ::  Env -> VarId -> MonoType -> Env
extendLocal env x t   =   MkEnv (Map.insert x (All [] t) (rep env))
extendGlobal          ::  Env -> VarId -> PolyType -> Env
extendGlobal env x t  =   MkEnv (Map.insert x t (rep env))
makeEnv               ::  [(VarId, PolyType)] -> Env
makeEnv               =   MkEnv . Map.fromList
unmakeEnv             ::  Env -> [(VarId, PolyType)]
unmakeEnv             =   Map.toList . rep
lookupEnv             ::  Env -> VarId -> PolyType
lookupEnv env x       =   fromJust (Map.lookup x (rep env))
domEnv                ::  Env -> [VarId]
domEnv env            =   Map.keys (rep env)
freeTVarEnv           ::  Env -> [TVarId]
freeTVarEnv env       =   concat (map freeTVarPoly (Map.elems (rep env)))
instance  Show Env  where
      showsPrec d  =  showsEnv
showsEnv              :: Shows Env
showsEnv              =  showsSurround "[" (showsStarSep ",\n " showsPair) "]"
                      .  unmakeEnv
showsPair             :: Shows (VarId, PolyType)
showsPair (x,t)       =  showsString x . showsString " : " . shows t
