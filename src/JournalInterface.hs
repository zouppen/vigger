{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
module JournalInterface (journalInterface) where

import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.STM
import Control.Monad (when)
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
import Control.Exception (AsyncException(..))

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
journalInterface unit recordingPath ops = bracket start stop $ \(_,h) -> loopify $ do
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

-- |Loop until we receive relevant exception and determine what to do
-- then. Less fatal exceptions are just printed out but others cause a
-- graceful exit.
loopify :: IO a -> IO ()
loopify act = do
  continue <- catches (act >> pure True) [vigger, ctrlc]
  when continue $ loopify act
  where
    vigger = Handler $ \e -> case e of
      ViggerStop -> do
        putStrLn "Quitting..."
        pure False
      ViggerNonFatal msg -> do
        putStrLn $ "Non-fatal error: " <> msg
        pure True
      ViggerEncodeFail code -> do
        putStrLn $ "Video encoding failed with exit code " <> show code <> ". Ignoring."
        pure True
    ctrlc = Handler $ \e -> do
      case e of
        UserInterrupt -> putStrLn "Quitting..."
        _ -> putStrLn $ "Got " <> show e <> " and quitting.."
      pure False

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
