module Main where

import Prelude
import System.Random
import System.Directory (createDirectory, removeDirectoryRecursive, removeFile, createDirectoryIfMissing)
import System.FilePath
import System.IO
import System.Environment (getArgs)
import Control.Monad (forever)
import Control.Concurrent.Async
import Control.Exception (mask_)

import qualified Repro2

main = do
  [dir] <- getArgs
  Repro2.repro dir

repro :: FilePath -> IO ()
repro dir' = do
  let dir = dir' </> "repro"
  createDirectoryIfMissing True dir
  forever $ do
    j <- randomRIO (1, 120) :: IO Int
    _ <- waitAnyCancel =<< (sequence . map (async . openCloseFileIn dir) $ [1..j])
    print (j,1)
    removeDirectoryRecursive dir
    createDirectory dir
    _ <- mapConcurrently (openCloseFileIn dir) $ [1..j]
    print (j,2)
    removeDirectoryRecursive dir
    createDirectory dir
  where
    openCloseFileIn dir i = do
      h <- openFile filepath WriteMode -- interrupting openFile may result in handle getting closed, but file remaining locked in ghc rts
      mask_ (removeFile filepath >> hClose h) -- mask it to minimize the number of handles that are left for GC to close, we don't want to hit the limit
      where filepath = dir </> "repro" ++ show i
