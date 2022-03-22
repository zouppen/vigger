{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Main where

import Data.Foldable (traverse_)

import CommandInterface
import Loader
import Config

main = do
  config <- parseCommandAndConfig
  withStuff $ \stuff -> do
    triggerOps <- initTriggers config stuff
    -- Handle incoming events
    putStrLn "Up and running..."
    cmdLoop stuff triggerOps
    -- Shutting down operations
    traverse_ shutdown triggerOps

