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
  { confFile :: FilePath
  } deriving (Show)


data Config = Config { triggers :: Map String Trigger
                     } deriving (Show, Generic)

data Trigger = Trigger { cameras :: Map String Camera
                       } deriving (Show, Generic)

data Camera = Camera { url        :: String
                     , precapture :: Int
                     } deriving (Show, Generic)

instance FromJSON Config where
   parseJSON = genericParseJSON strictOptions

instance FromJSON Trigger where
  parseJSON = genericParseJSON strictOptions

instance FromJSON Camera where
  parseJSON = genericParseJSON strictOptions

optParser :: Parser Options
optParser = Options
  <$> strOption ( mempty
                  <> short 'c'
                  <> long "config"
                  <> metavar "FILE"
                  <> help "Configuration file in YAML format"
                )

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
