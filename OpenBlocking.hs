module Main where

import System.IO
import GHC.IO.Handle.FD
import Control.Concurrent
import Control.Exception

main = do
  putStrLn "try open"
  openFileBlocking "fifo" WriteMode
  putStrLn "Done"
