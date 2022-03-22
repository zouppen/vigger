{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Main where

import Data.Foldable (traverse_)

import CommandInterface
import Loader
import Config

main = do
  Options{..} <- parseCommandAndConfig
  withStuff $ \stuff -> do
    triggerOps <- initTriggers config stuff
    -- Handle incoming events
    putStrLn "Up and running..."
    case interface of
      Cli -> cli triggerOps
      Unit name -> do
        putStrLn $ "Systemd unit not yet supported. " <> name
    -- Shutting down operations
    traverse_ shutdown triggerOps

