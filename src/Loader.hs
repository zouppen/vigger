{-# LANGUAGE RecordWildCards #-}
module Loader ( Stuff(..)
              , TriggerOp(..)
              , TriggerData(..)
              , withStuff
              , initTriggers
              ) where

import Control.Applicative
import Control.Concurrent (killThread, forkIO)
import Control.Concurrent.STM
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Time.LocalTime (ZonedTime, getZonedTime)
import System.INotify (INotify, withINotify)
import Data.Text.Lazy (Text, pack, unpack)

import Config
import Deadman
import Exceptions
import Ffmpeg
import Trasher
import Watch
import Matrix
import VideoTools (TriggerData(..), snapshotFrame)

-- |Initialized stuff, intended to be used as a singleton, initialized
-- with `withStuff`.
data Stuff = Stuff { ih         :: INotify
                   , trash      :: Trash
                   , matrixConn :: MatrixConn
                   }

-- |Camera consists of video splitter and file watcher
data CameraOp = CameraOp { watch        :: Watch
                         , splitterStop :: IO ()
                         }

-- |Operations for a single trigger.
data TriggerOp = TriggerOp { motionStart  :: IO ()
                           , motionEnd    :: IO TriggerData
                           , shutdown     :: IO ()
                           , trigSnapshot :: IO (Map Text Jpeg)
                           , cameraState  :: IO (Map Text FileEvent)
                           }

-- |Initialize all the bells and whistles and run the action.
withStuff :: Config -> (Stuff -> IO ()) -> IO ()
withStuff Config{..} act = do
  matrixConn <- initMatrixConn matrix
  withINotify $ \ih -> do
    withTrasher $ \trash -> do
      act Stuff{..}

-- |Initialize everything for the command interface, with a map
-- of possible operations.
initTriggers :: Config -> Stuff -> IO (Map Text TriggerOp)
initTriggers Config{..} stuff = M.traverseWithKey (forkTrigger stuff) triggers

-- |Fork a trigger, which is be composed of one or more cameras.
forkTrigger :: Stuff -> Text -> Trigger -> IO TriggerOp
forkTrigger stuff triggerName Trigger{..} = do
  ops <- traverse (forkCamera stuff) cameras
  startVar <- newEmptyTMVarIO
  -- Collect actions
  let motionStart = do
        now <- getZonedTime
        atomically $ do
          putTMVar startVar now <|> throwSTM (ViggerNonFatal "Already triggered")
          traverse_ (startCapture.watch) ops
      motionEnd = atomically $ do
        startTime <- takeTMVar startVar <|> throwSTM (ViggerNonFatal "Trigger already stopped")
        videoFiles <- traverse (stopCapture.watch) ops
        pure TriggerData{..}
      shutdown = flip traverse_ ops $ \CameraOp{..} -> do
        -- Stop FFmpeg
        splitterStop
        -- Stop watches
        stopWatch watch
      cameraState = atomically $ traverse (lastEnqueue . watch) ops
      trigSnapshot = do
        let rot camOp = maybe 0 id $ rotate $ watchCamera $ watch camOp 
        let files = M.map (\camOp -> (rot camOp, fmap path $ lastEnqueue $ watch camOp)) ops
        snapshotFrame files
  pure TriggerOp{..}

-- |Fork video splitter and cleaner for an individual camera.
forkCamera :: Stuff -> Camera -> IO CameraOp
forkCamera Stuff{..} cam@Camera{..} = do
  -- Start change watcher
  watch <- forkWatch trash cam ih
  -- Start video splitter with FFmpeg
  let start = startVideoSplit (workDir watch) url
  splitterStop <- case timeout of
    Just timeout -> do
      tid <- forkIO $ deadManLoop
        (toEnum timeout)
        (time <$> (atomically $ lastEnqueue watch))
        start
        stopVideoSplit
      pure $ killThread tid
    Nothing -> do
      procH <- start
      pure $ stopVideoSplit procH
  -- Return handle
  pure CameraOp{..}
