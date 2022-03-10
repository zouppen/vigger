{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Main where

import System.Posix.Env.ByteString
import System.INotify
import Control.Concurrent.STM
import Watch

main = do
  [dir] <- getArgs
  (_, trash) <- runTrasher
  withINotify $ \ih -> do
    w <- forkWatch trash maxAge ih dir
    cmdLoop w
    stopWatch trash w
  pure ()
  where maxAge = 5

cmdLoop :: Watch -> IO ()
cmdLoop w = do
  cmd <- getLine
  case cmd of
    "start" -> do
      atomically $ startMotion w
      putStrLn "Motion started"
      cmdLoop w
    "stop" -> do
      files <- atomically $ stopMotion w
      putStrLn $ "Motion stopped " ++ show files
      cmdLoop w
    "quit" -> do
      putStrLn "Quitting"
    _ -> do
      putStrLn "Unknown command"
      cmdLoop w

