module Main where

import System.IO
import GHC.IO.Handle.FD
import Control.Concurrent

main = do
  opener <- forkIO $ do
    putStrLn "try open"
    openFileBlocking "fifo" WriteMode
    putStrLn "Done"
  threadDelay $ 10^6
  putStrLn "kill"
  killThread opener
  putStrLn "killed"

-- on linux compiled with ghc 8.8.3 it can't kill the thread blocked on
-- opening
-- on linux compiled with ghc head, the thread can be killed
