{-# LANGUAGE RecordWildCards, DeriveGeneric, CPP #-}
module Config ( Config(..)
              , Options(..)
              , Trigger(..)
              , Camera(..)
              , Interface(..)
              , parseCommandAndConfig
              ) where

import GHC.Generics
import Data.Map.Strict (Map)
import Data.Text.Lazy (Text)
import Data.Yaml ( FromJSON
                 , ParseException
                 , prettyPrintParseException
                 , decodeFileEither
                 , parseJSON
                 )
import Data.Aeson.Types hiding (Options, Parser)
import Options.Applicative
import System.Exit (die)

compatOptions = defaultOptions{ fieldLabelModifier = camelTo2 '_'}
#if MIN_VERSION_aeson(1,4,7)
strictOptions = compatOptions{ rejectUnknownFields = True }
#else
strictOptions = compatOptions
#endif

data Options = Options
  { config    :: Config
  , confFile  :: FilePath
  , interface :: Interface
  } deriving (Show)

data Interface = Cli         -- ^Command line interface
               | Unit String -- ^Systemd journal unit
               deriving (Show)

data Config = Config { recordingPath :: Text
                     , matrix        :: Maybe Value
                     , triggers      :: Map Text Trigger
                     } deriving (Show, Generic)

data Trigger = Trigger { cameras :: Map Text Camera
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

-- |Returns Options parser, leave a hole in the config.
optParser :: Parser Options
optParser = Options undefined <$> config <*> (cli <|> unit)
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

-- |Parses command line options
parseCommandAndConfig :: IO Options
parseCommandAndConfig = do
  Options{..} <- execParser opts
  config <- decodeFileEither confFile >>= yamlError confFile
  pure $ Options{..}

-- |Dies if it has a ParseException or continues execution otherwise.
yamlError :: String -> Either ParseException a -> IO a
yamlError _ (Right a) = pure a
yamlError name (Left e)  = die $
  "Error while reading " <> name <> ":\n" <> prettyPrintParseException e
