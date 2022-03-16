{-# LANGUAGE RecordWildCards, OverloadedStrings, DeriveGeneric #-}
module Main where

import qualified Data.ByteString as B
import System.INotify
import Control.Concurrent.STM
import Options.Applicative
import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text)
import Data.Map.Strict
import GHC.Generics
import Data.Yaml hiding (Parser)
import System.Exit (die)
import Watch
import Trasher

data Options = Options
  { confFile :: FilePath
  } deriving (Show)


data Config = Config { triggers :: Map String Trigger
                     } deriving (Show, Generic)

data Trigger = Trigger { cameras :: Map String Camera
                       } deriving (Show, Generic)

data Camera = Camera { url        :: String
                     , precapture :: Int
                     } deriving (Show, Generic)


instance FromJSON Config

instance FromJSON Trigger

instance FromJSON Camera

optParser :: Parser Options
optParser = Options
  <$> strOption ( mempty
                  <> short 'c'
                  <> long "config"
                  <> metavar "FILE"
                  <> help "Configuration file in YAML format"
                )

opts = info (optParser <**> helper)
       ( fullDesc
         <> header ("vigger - Video trigger for FFmpeg compatible cameras")
       )

main = do
  Options{..} <- execParser opts
  conf <- decodeFileEither confFile >>= yamlError confFile
  
  print (conf :: Config)

-- |Dies if it has a ParseException or continues execution otherwise.
yamlError :: String -> Either ParseException a -> IO a
yamlError _ (Right a) = pure a
yamlError name (Left e)  = die $
  "Error while reading " ++ name ++ ":\n" ++ prettyPrintParseException e

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

