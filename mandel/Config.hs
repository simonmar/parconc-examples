{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE TemplateHaskell #-}

module Config (

  Options, optBackend, optSize, optLimit, optBench,
  processArgs, run, run1

) where

import Data.Label
import System.Exit
import System.Console.GetOpt
import Data.Array.Accelerate                            ( Arrays, Acc )
import qualified Data.Array.Accelerate.Interpreter      as Interp
#ifdef ACCELERATE_CUDA_BACKEND
import qualified Data.Array.Accelerate.CUDA             as CUDA
#endif

data Backend = Interpreter
#ifdef ACCELERATE_CUDA_BACKEND
             | CUDA
#endif
  deriving (Bounded, Show)

data Options = Options
  {
    optBackend         :: Backend
  , optSize            :: Int
  , optLimit           :: Int
  , optBench           :: Bool
  , optHelp            :: Bool
  }
  deriving Show

defaultOptions :: Options
defaultOptions = Options
  { optBackend         = maxBound
  , optSize            = 512
  , optLimit           = 255
  , optBench           = False
  , optHelp            = False
  }


run :: Arrays a => Options -> Acc a -> a
run opts = case optBackend opts of
  Interpreter   -> Interp.run
#ifdef ACCELERATE_CUDA_BACKEND
  CUDA          -> CUDA.run
#endif

run1 :: (Arrays a, Arrays b) => Options -> (Acc a -> Acc b) -> a -> b
run1 opts f = case optBackend opts of
  Interpreter   -> head . Interp.stream f . return
#ifdef ACCELERATE_CUDA_BACKEND
  CUDA          -> CUDA.run1 f
#endif


options :: [OptDescr (Options -> Options)]
options =
  [ Option []   ["interpreter"] (NoArg  (\o -> o{optBackend=Interpreter}))   "reference implementation (sequential)"
#ifdef ACCELERATE_CUDA_BACKEND
  , Option []   ["cuda"]        (NoArg  (\o -> o{optBackend = CUDA}))          "implementation for NVIDIA GPUs (parallel)"
#endif
  , Option []   ["size"]        (ReqArg (\i o -> o{optSize = read i}) "INT")     "visualisation size (512)"
  , Option []   ["limit"]       (ReqArg (\i o -> o{optLimit = read i}) "INT")    "iteration limit for escape (255)"
  , Option []   ["benchmark"]   (NoArg  (\o -> o{optBench = True}))            "benchmark instead of displaying animation (False)"
  , Option "h?" ["help"]        (NoArg  (\o -> o{optHelp = True}))             "show help message"
  ]


processArgs :: [String] -> IO (Options, [String])
processArgs argv =
  case getOpt' Permute options argv of
    (o,_,n,[])  -> case foldl (flip id) defaultOptions o of
                     opts | False <- optHelp opts   -> return (opts, n)
                     opts | True  <- optBench opts  -> return (opts, "--help":n)
                     _                                  -> putStrLn (helpMsg []) >> exitSuccess
    (_,_,_,err) -> error (helpMsg err)
  where
    helpMsg err = concat err ++ usageInfo header options
    header      = unlines
      [ "accelerate-mandelbrot (c) [2011..2012] The Accelerate Team"
      , ""
      , "Usage: accelerate-mandelbrot [OPTIONS]"
      ]

