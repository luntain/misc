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
  threadDelay $ 10^12
