{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
module JournalInterface (journalInterface) where

import Control.Concurrent.STM
import Control.Monad (when)
import Data.Aeson hiding (Options)
import Data.Function (fix)
import Data.Map.Strict (Map, (!?))
import Data.Text.Lazy (Text, unpack)
import GHC.Generics
import System.IO (Handle, hClose)
import System.Process
import qualified Data.ByteString as B

import Config (Config(..))
import Exceptions
import Loader
import Watch
import VideoTools (renderVideos)

data Rpc = Rpc { method :: Text
               , params :: Bool
               } deriving (Generic, Show)

instance FromJSON Rpc

-- Handle incoming RPC messages from journalctl
journalInterface :: Config -> String -> Map Text TriggerOp -> IO ()
journalInterface config unit ops = bracket start stop $ \(_,h) -> viggerLoopCatch $ do
  Rpc{..} <- takeNextJsonLine h
  let op = ops !? method
  case (op, params) of
    (Just TriggerOp{..}, True) -> do
      motionStart
      putStrLn $ "Motion started on " <> unpack method
    (Just TriggerOp{..}, False) -> do
      td@TriggerData{..} <- motionEnd
      videos <- renderVideos config td
      putStrLn $ "Motion stopped on " <> unpack method <> ". Got videos: " <> show videos
    _ -> putStrLn $ "Ignored method " <> unpack method
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
