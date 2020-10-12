{-# Language LambdaCase,ScopedTypeVariables #-}
module Repro2 where

import Prelude
import System.Directory
import System.FilePath
import System.IO
import System.Environment (getArgs)
import Control.Monad (forever)
import Control.Exception
import Control.Concurrent
import Data.Time
import Control.Monad (when)


repro :: FilePath -> IO ()
repro dir' = do
  let dir = dir' </> "repro"
  createDirectoryIfMissing True dir
  availableNames <- newChan :: IO (Chan FilePath)
  writeList2Chan availableNames $ [ dir </> "repro" ++ show (i :: Int) | i <- [1..10]]
  toClose <- newChan :: IO (Chan (Handle, FilePath))
  maybeDelete <- newChan :: IO (Chan FilePath)
  forkIO (getChanContents maybeDelete >>= mapM_ (keepDeleting availableNames))
  forkIO (getChanContents toClose >>= mapM_ (keepClosing availableNames))
  openingThread <- keepOpening availableNames toClose maybeDelete `forkFinally`
            \case
              Left e -> print e
              Right () -> print "impossible"
  forever $ do
    threadDelay (10^3)
    throwTo openingThread ThreadKilled


keepOpening :: Chan FilePath -> Chan (Handle, FilePath) -> Chan FilePath -> IO ()
keepOpening availableNames toClose maybeDelete = do
  uninterruptibleMask $ \ restore -> do
    forever $ do
      filepath <- readChan availableNames
      now <- getCurrentTime
      h <- (Just <$> restore (openFile filepath WriteMode)) `catch` \(e :: AsyncException) ->
          if e == ThreadKilled then do
            putStrLn "interrupt"
            writeChan maybeDelete filepath >> pure Nothing
          else do
            putStrLn "Other exc"
            print e
            throw e
      elapsed <- (`diffUTCTime` now) <$> getCurrentTime
      print elapsed
      case h of
        Nothing -> pure ()
        Just h -> writeChan toClose (h, filepath)


keepDeleting :: Chan FilePath -> FilePath -> IO ()
keepDeleting availableNames name = do
  exist <- doesFileExist name
  when exist $ removeFile name
  writeChan availableNames name

keepClosing :: Chan FilePath -> (Handle, FilePath) -> IO ()
keepClosing availableNames (handle, name) = do
  hClose handle
  removeFile name
  writeChan availableNames name
