module Main where

import System.IO
import GHC.IO.Handle.FD
import Control.Concurrent
import Control.Exception

main = do
  putStrLn "try open"
  openFileBlocking "fifo" WriteMode
  putStrLn "Done"

-- on linux, copiled with 8.8.3 two ctrl-c's are needed to kill the
-- program blocked on opening
--
-- on linux with ghc head one ctrl-c is enough
-- on linux with with T18832 fix, it regresses to needing two ctrl-c's to kill the program
