{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
module JournalInterface (journalInterface) where

import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.STM (atomically)
import Control.Exception (try, throw, bracket)
import Control.Monad (forever)
import Data.Aeson
import qualified Data.ByteString as B
import Data.Function (fix)
import Data.Map.Strict (Map, (!?), keys, foldlWithKey)
import GHC.Generics
import System.IO (Handle, hClose)
import System.IO.Temp (emptySystemTempFile)
import System.Process

import Exceptions
import Ffmpeg
import Loader
import Watch

data Rpc = Rpc { method :: String
               , params :: Bool
               } deriving (Generic, Show)

instance FromJSON Rpc

-- Handle incoming RPC messages from journalctl
journalInterface :: String -> Map String TriggerOp -> IO ()
journalInterface unit ops = bracket start stop $ \(_,h) -> forever $ do
  Rpc{..} <- takeNextJsonLine h
  let op = ops !? method
  case (op, params) of
    (Just TriggerOp{..}, True) -> do
      motionStart
      putStrLn $ "Motion started on " <> method
    (Just TriggerOp{..}, False) -> do
      -- Encode video and remove files afterwards
      motion <- motionEnd
      videos <- forConcurrently motion $ \Capture{..} -> do
        outfile <- emptySystemTempFile "vigger.mp4"
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
