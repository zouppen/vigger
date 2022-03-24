{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Main where

import Data.Foldable (traverse_)

import CommandInterface
import JournalInterface
import Loader
import Config

main = do
  Options{..} <- parseCommandAndConfig
  withStuff $ \stuff -> do
    triggerOps <- initTriggers config stuff
    -- Handle incoming events
    putStrLn "Up and running..."
    case interface of
      Cli -> commandInterface triggerOps
      Unit name -> journalInterface name triggerOps
    -- Shutting down operations
    traverse_ shutdown triggerOps

