--
-- Adapted from the program "infer", believed to have been originally
-- authored by Philip Wadler, and used in the nofib benchmark suite
-- since at least the late 90s.
--

module Main where

import Parse
import Lex
import Term
import Type
import Environment
import InferMonad
import Infer
import  Control.Monad.Par.Scheds.Trace
import System.IO
import System.Exit (exitWith, ExitCode(..))
import qualified Data.Map as Map

main :: IO ()
main =  do
  l <- getContents
  case parseBinds (alexScanTokens l) of
    Left err -> die err
    Right t  -> print (inferBinds initialEnv t)

die :: String -> IO ()
die s = hPutStrLn stderr s >> exitWith (ExitFailure 1)

test :: String -> IO ()
test str =
  case parseExp (alexScanTokens str) of
    Left err -> die err
    Right t  -> print (useI (error "type error") $ inferTerm initialEnv t)

inferBinds :: Env -> [(VarId,Term)] -> [(VarId,PolyType)]
inferBinds e t = runPar $ do
  ys <- mapM (\(x,ty) -> do v <- newFull ty; return (x,v)) (unmakeEnv e)
  let topenv = Map.fromList ys
  inferTop topenv t

initialEnv :: Env
initialEnv = foldl (uncurry . extendGlobal) emptyEnv types
 where
  types = [("+",intop),("*",intop),("-",intop),("/",intop)]
  intop = All [] (intType `arrow` intType `arrow` intType)
