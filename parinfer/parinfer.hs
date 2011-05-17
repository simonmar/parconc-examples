module Main where

import Parse
import Shows
import Term
import Type
import Environment
import InferMonad
import Substitution	( Sub )
import MaybeM		( Maybe )
import Infer
import Control.Monad.Par
import System.IO
import System.Exit
import qualified Data.Map as Map
import Control.DeepSeq
import Control.Exception

main =  do
  l <- getContents
  case runP (lexactlyP reads) l of
    Nothing -> die "failed to parse"
    Just t  -> do
      let r = runPar (inferTop Map.empty t)
      evaluate (deep r)

deep x = deepseq x x

die s = hPutStrLn stderr s >> exitWith (ExitFailure 1)
