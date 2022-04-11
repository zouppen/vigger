{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Main where

import Data.Foldable (traverse_)

import CommandInterface
import JournalInterface
import SimpleInterface
import Loader
import Config

main = do
  Options{..} <- parseCommandAndConfig
  withStuff config $ \stuff -> do
    triggerOps <- initTriggers config stuff
    -- Handle incoming events
    putStrLn "Up and running..."
    case interface of
      Cli -> commandInterface config (matrixConn stuff) triggerOps
      Unit unit -> journalInterface config (matrixConn stuff) unit triggerOps
      Simple -> simpleInterface config (matrixConn stuff) triggerOps
    -- Shutting down operations
    traverse_ shutdown triggerOps

