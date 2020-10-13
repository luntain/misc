module Main where

import System.Environment (getArgs)
import qualified Repro2

main = do
  [dir] <- getArgs
  Repro2.repro dir
