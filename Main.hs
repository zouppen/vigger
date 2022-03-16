{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Main where

import qualified Data.ByteString as B
import System.INotify
import Control.Concurrent.STM
import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text)

import Config
import Watch
import Trasher


main = do
  conf <- parseCommandAndConfig
  print conf

{-
  [dir] <- getArgs
  (_, trash) <- runTrasher
  withINotify $ \ih -> do
    w <- forkWatch trash maxAge ih dir
    cmdLoop w
    stopWatch trash w
  pure ()
  where maxAge = 5
-}

cmdLoop :: Watch -> IO ()
cmdLoop w = do
  cmd <- getLine
  case cmd of
    "start" -> do
      atomically $ startCapture w
      putStrLn "Motion started"
      cmdLoop w
    "stop" -> do
      files <- atomically $ stopCapture w
      putStrLn $ "Motion stopped " ++ show files
      cmdLoop w
    "quit" -> do
      putStrLn "Quitting"
    _ -> do
      putStrLn "Unknown command"
      cmdLoop w
