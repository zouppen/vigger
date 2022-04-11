{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module SimpleInterface (simpleInterface) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (when)
import Data.Function (fix)
import Data.Map.Strict (Map, (!?), traverseWithKey)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import GHC.Generics
import System.IO (Handle, hClose)
import System.Process
import qualified Data.ByteString as B

import Config (Config(..), Matrix(..))
import Exceptions
import Loader
import Watch
import VideoTools (renderVideos)
import Matrix
import Ffmpeg (jpegPayload)

-- Handle incoming RPC messages from journalctl
simpleInterface :: Config -> MatrixConn -> Map Text TriggerOp -> IO ()
simpleInterface config matrixConn ops = viggerLoopCatch $ do
  line <- T.getLine
  case T.uncons line of
    Just (mode, trigger) -> case ops !? trigger of
      Just TriggerOp{..} -> case mode of
        '1' -> do
          motionStart
          T.putStrLn $ "Motion started on " <> trigger
          forkIO $ do
            -- Triggering Matrix message in a single thread
            let matrixRoom = room $ matrix config
            payload <- trigSnapshot
            out <- flip traverseWithKey payload $ \cam jpeg ->
              sendImage matrixConn matrixRoom cam (jpegPayload jpeg)
            T.putStrLn $ "Triggered Matrix messages: " <> T.pack (show out)
          pure ()
        '0' -> do
          td@TriggerData{..} <- motionEnd
          videos <- renderVideos config td
          T.putStrLn $ "Motion stopped on " <> trigger <> ". Got videos: " <> T.pack (show videos)
        _ -> T.putStrLn $ "Mode should be 0 or 1, got " <> T.singleton mode
      _ -> T.putStrLn $ "Trigger not found: " <> trigger
    _ -> T.putStrLn $ "Invalid command: " <> line
