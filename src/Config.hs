{-# LANGUAGE RecordWildCards, DeriveGeneric #-}
module Config ( Config(..)
              , Trigger(..)
              , Camera(..)
              , parseCommandAndConfig
              ) where

import GHC.Generics
import Data.Map.Strict (Map)
import Data.Yaml ( FromJSON
                 , ParseException
                 , prettyPrintParseException
                 , decodeFileEither
                 , parseJSON
                 )
import Data.Aeson.Types ( defaultOptions
                        , rejectUnknownFields
                        , genericParseJSON
                        )
import Options.Applicative
import System.Exit (die)

strictOptions = defaultOptions{ rejectUnknownFields = True }

data Options = Options
  { confFile  :: FilePath
  , interface :: Interface
  } deriving (Show)

data Interface = Cli         -- ^Command line interface
               | Unit String -- ^Systemd journal unit
               deriving (Show)

data Config = Config { triggers :: Map String Trigger
                     } deriving (Show, Generic)

data Trigger = Trigger { cameras :: Map String Camera
                       } deriving (Show, Generic)

data Camera = Camera
  { url        :: String    -- ^Video source of the camera. May be
                            -- anything supported by FFmpeg.
  , precapture :: Int       -- ^How many seconds of prerecorded video
                            -- to keep in case of motion detection
  , timeout    :: Maybe Int -- ^Timeout in seconds (max interval
                            -- between keyframes). If Nothing, then no
                            -- dead man switch used
  } deriving (Show, Generic)

instance FromJSON Config where
   parseJSON = genericParseJSON strictOptions

instance FromJSON Trigger where
  parseJSON = genericParseJSON strictOptions

instance FromJSON Camera where
  parseJSON = genericParseJSON strictOptions

optParser :: Parser Options
optParser =  Options <$> config <*> (cli <|> unit)
  where
    config = strOption ( short 'c' <>
                         long "config" <>
                         metavar "FILE" <>
                         help "Configuration file in YAML format" )
    unit = Unit <$> strOption ( short 'u' <>
                                long "unit" <>
                                metavar "NAME" <>
                                help "Systemd unit name" )
    cli = flag' Cli (short 'i' <> long "cli")

opts :: ParserInfo Options
opts = info (optParser <**> helper)
       ( fullDesc
         <> header ("vigger - Video trigger for FFmpeg compatible cameras")
       )

parseCommandAndConfig :: IO Config
parseCommandAndConfig = do
  Options{..} <- execParser opts
  decodeFileEither confFile >>= yamlError confFile

-- |Dies if it has a ParseException or continues execution otherwise.
yamlError :: String -> Either ParseException a -> IO a
yamlError _ (Right a) = pure a
yamlError name (Left e)  = die $
  "Error while reading " ++ name ++ ":\n" ++ prettyPrintParseException e
