{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
module JournalInterface (journalInterface) where

import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.STM
import Control.Monad (forever)
import Data.Aeson hiding (Options)
import qualified Data.ByteString as B
import Data.Function (fix)
import Data.Map.Strict (Map, (!?), keys, foldlWithKey, insert, toList, delete)
import GHC.Generics
import System.IO (Handle, hClose)
import System.IO.Temp (emptySystemTempFile)
import System.Process
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)

import Exceptions
import Ffmpeg
import Loader
import Watch

data Rpc = Rpc { method :: String
               , params :: Bool
               } deriving (Generic, Show)

instance FromJSON Rpc

-- Handle incoming RPC messages from journalctl
journalInterface :: String -> String -> Map String TriggerOp -> IO ()
journalInterface unit recordingPath ops = do
  bracket start stop $ \(_,h) -> forever $ do
    Rpc{..} <- takeNextJsonLine h
    let op = ops !? method
    case (op, params) of
      (Just TriggerOp{..}, True) -> do
        motionStart
        putStrLn $ "Motion started on " <> method
      (Just TriggerOp{..}, False) -> do
        TriggerData{..} <- motionEnd
        let date = formatTime defaultTimeLocale "%Y-%m-%d" startTime
        let time = formatTime defaultTimeLocale "%H%M%S%z" startTime
        videos <- forConcurrently (toList videoFiles) $ \(name, Capture{..}) -> do
          -- Prepare directory hierarchy for the video file
          let dir = recordingPath </> date </> name
          createDirectoryIfMissing True dir
          let outfile = dir </> time <> ".mp4"
          -- Encode video and remove files afterwards
          composeVideo outfile captureFiles
          atomically captureClean
          pure outfile
        putStrLn $ "Motion stopped on " <> method <> ". Got videos: " <> show videos
      _ -> putStrLn $ "Ignored method " <> method
  where
    start = runJournal False unit
    stop (procH, h) = do
      hClose h
      terminateProcess procH
      waitForProcess procH
      pure ()

takeKey :: Ord k => TVar (Map k a) -> k -> STM (Maybe a)
takeKey var k = do
  m <- readTVar var
  writeTVar var $ delete k m
  pure $ m !? k

-- |Gets lines until a valid JSON element is found, ignoring other
-- input. In case of EOF ViggerStop is thrown.
takeNextJsonLine :: FromJSON a => Handle -> IO a
takeNextJsonLine h = fix $ \loop -> get >>= maybe loop pure . decodeStrict   
  where get = do
          line <- try $ B.hGetLine h
          either (ioeConst $ throw ViggerStop) pure line

-- |Run journalctl with given unit.
runJournal :: Bool -> String -> IO (ProcessHandle, Handle)
runJournal isUserUnit unit = do
  (_, Just h, _, procH) <- createProcess cp
  pure (procH, h)
  where cp = (proc "journalctl" args){ std_in = NoStream
                                     , std_out = CreatePipe
                                     , close_fds = True
                                     }
        args = ["-n", "0", "-f", "-o", "cat", if isUserUnit then "--user-unit" else "-u", unit]
