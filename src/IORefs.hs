module IORefs where

import Data.IORef

prog :: IO ()
prog = do
  ref <- newIORef 0
  incRef ref
  incRef ref
  incRef ref
  val <- readIORef ref
  print val

incRef ref =
  modifyIORef ref (+ 1)
