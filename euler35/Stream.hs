{-# LANGUAGE BangPatterns, CPP #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fwarn-unused-imports #-}
-- -Wall 

-- A module for stream processing built on top of Control.Monad.Par

-- (In the future may want to look into the stream interface used by
--  the stream fusion framework.)

module Stream
 ( 
   Stream, streamFromList, streamMap, streamScan, streamFold, streamFilter
 ) where

import Control.Monad.Par as P
import Control.Monad.Par.IList
import Control.DeepSeq

--------------------------------------------------------------------------------
-- Types

type Stream a = IVar (IList a)

-- -----------------------------------------------------------------------------
-- Stream operators

streamFromList :: NFData a => [a] -> Par (Stream a)
streamFromList xs = do
  var <- new
  fork $ loop xs var
  return var
 where
  loop [] var = put var Null
  loop (x:xs) var = do
    tail <- new
    put var (Cons x tail)
    loop xs tail

-- This version exposes pipeline parallelism but no data parallelism.
-- It shouldn't be necessary if fork is sufficiently efficient and if
-- work stealing is done right.
streamMap :: NFData b => (a -> b) -> Stream a -> Par (Stream b)
streamMap fn instrm = 
    do outstrm <- new
       fork$ loop instrm outstrm
       return outstrm
 where
  loop instrm outstrm = 
   do 
      ilst <- get instrm
      case ilst of 
	Null -> put outstrm Null -- End of stream.
	Cons h t -> 
	  do newtl <- new
	     put outstrm (Cons (fn h) newtl)
	     loop t newtl


-- | Applies a stateful kernel to the stream.  Output stream elements match input one-to-one.
-- streamScan :: (NFData b, NFData c) => 
streamScan :: (NFData a, NFData b, NFData c) =>  -- <- TEMP, don't need NFData a in general.
	      (a -> b -> (a,c)) -> a -> Stream b -> Par (Stream c)
streamScan fn initstate instrm = 
    do outstrm <- new
       fork$ loop initstate instrm outstrm
       return outstrm
 where
#ifdef DEBUGSTREAMS
  -- Create a task log for each unique input stream fed to this function:
  tasklog = unsafeNewTaskSeries (nameFromValue instrm)
#endif

  loop state instrm outstrm = 
   do 
      ilst <- get instrm
      case ilst of 
	Null -> put outstrm Null -- End of stream.
	Cons h t -> 
	  do newtl <- new
	     let (newstate, outp) = 
#ifdef DEBUGSTREAMS
		                    timePure tasklog$ fn state h
#else
		                    fn state h
#endif
	     put outstrm (Cons outp newtl)
	     loop newstate t newtl


-- | Reduce a stream to a single value.  This function will not return
--   until it reaches the end-of-stream.
streamFold :: (a -> b -> a) -> a -> Stream b -> Par a
streamFold fn acc instrm = 
   do ilst <- get instrm
      case ilst of 
	Null     -> return acc 
	Cons h t -> streamFold fn (fn acc h) t 

streamFilter :: NFData a => (a -> Bool) -> Stream a -> Par (Stream a)
streamFilter p instr = do
    outstr <- new
    fork $ loop instr outstr
    return outstr
  where
    loop instr outstr = do
      v <- get instr
      case v of
        Null -> put outstr Null
        Cons x instr'
          | p x -> do
             tail <- new
             put_ outstr (Cons x tail)
             loop instr' tail
          | otherwise -> do
             loop instr' outstr

