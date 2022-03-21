{-# LANGUAGE OverloadedStrings #-}
module Ffmpeg (startVideoSplit, stopVideoSplit, ProcessHandle) where

import System.IO
import System.Process
import qualified Data.ByteString.Lazy.Char8 as B
import System.IO.Temp (withSystemTempFile)
import System.FilePath.ByteString (RawFilePath)
import System.Exit
import Control.Exception (throw)

import Exceptions

-- |Concatenates video using FFmpeg concat demuxer. May throw
-- ViggerException in case FFmpeg returns non-zero return value.
composeVideo :: Foldable t => FilePath -> t RawFilePath -> IO ()
composeVideo outfile infiles = withSystemTempFile "vigger" $ \path h -> do
  p <- runFfmpeg Nothing [ "-y", "-nostdin"
                         , "-loglevel", "warning"
                         , "-f", "concat"
                         , "-safe", "0"
                         , "-i", path
                         , "-c", "copy"
                         , outfile
                         ]
  val <- waitForProcess p
  case val of
    ExitSuccess -> pure ()
    ExitFailure x -> throw $ ViggerEncodeFail x
  where
    header = "ffconcat version 1.0\n# Automatically generated by vigger"
    escape "" = error "Empty filename given"
    escape bs = B.intersperse '\\' bs
    addFile a b = a <> "\nfile \\" <> escape (B.fromStrict b)
    payload = foldl addFile header infiles <> "\n"

-- |Stop splitting the video
stopVideoSplit :: ProcessHandle -> IO ()
stopVideoSplit h = do
  terminateProcess h
  waitForProcess h
  pure ()

-- |Start splitting the video and output to given directory
startVideoSplit :: FilePath -> String -> IO ProcessHandle
startVideoSplit dir video = runFfmpeg (Just dir)
  [ "-y", "-nostdin"
  , "-rtsp_transport", "tcp"
  , "-loglevel", "warning"
  , "-i", video
  , "-f", "segment"
  , "-c", "copy"
  , "tmp-%d.mp4"
  ]

-- |Run FFmpeg with given working directory and arguments. Makes sure stdin is not read.
runFfmpeg :: Maybe FilePath -> [String] -> IO ProcessHandle
runFfmpeg cwd args = do
  (_, _, _, h) <- createProcess cp
  pure h
  where cp = (proc "ffmpeg" args){ cwd = cwd
                                 , std_in = NoStream
                                 , close_fds = True
                                 }
