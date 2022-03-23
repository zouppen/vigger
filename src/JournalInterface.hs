{-# LANGUAGE DeriveGeneric #-}
module JournalInterface (journalInterface) where

import Data.Aeson
import System.IO (Handle, hClose)
import System.Process
import qualified Data.ByteString as B
import GHC.Generics
import Data.Function (fix)
import Control.Exception (bracket)
import Data.Map.Strict (Map, (!?), keys, foldlWithKey)
import Control.Monad (forever)

import Loader

data Rpc = Rpc { method :: String
               , params :: Bool
               } deriving (Generic, Show)

instance FromJSON Rpc

-- Handle incoming RPC messages from journalctl
journalInterface :: String -> Map String TriggerOp -> IO ()
journalInterface unit ops = bracket start stop loop
  where
    start = runJournal False unit
    stop (procH, h) = do
      hClose h
      terminateProcess procH
      waitForProcess procH
      pure ()
    loop (_, h) = forever $ do
      x <- takeNextJsonLine h
      print (x :: Rpc)

-- |Gets lines until a valid JSON element is found, ignoring other
-- input. In case of EOF we get IOException.
takeNextJsonLine :: FromJSON a => Handle -> IO a
takeNextJsonLine h = fix $ \loop -> B.hGetLine h >>= maybe loop pure . decodeStrict   

-- |Run journalctl with given unit.
runJournal :: Bool -> String -> IO (ProcessHandle, Handle)
runJournal isUserUnit unit = do
  (_, Just h, _, procH) <- createProcess cp
  pure (procH, h)
  where cp = (proc "journalctl" args){ std_in = NoStream
                                     , std_out = CreatePipe
                                     , close_fds = True
                                     }
        args = ["-f", "-o", "cat", if isUserUnit then "--user-unit" else "-u", unit]
