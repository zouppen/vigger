module Ffmpeg (startVideoSplit, stopVideoSplit, ProcessHandle) where

import System.Process

-- |Stop splitting the video
stopVideoSplit :: ProcessHandle -> IO ()
stopVideoSplit h = do
  terminateProcess h
  waitForProcess h
  pure ()

-- |Start splitting the video and output to given directory
startVideoSplit :: FilePath -> String -> IO ProcessHandle
startVideoSplit dir video = do
  (_, _, _, h) <- createProcess $ ffmpegSplitProc dir video
  pure h

ffmpegSplitProc :: FilePath -> String -> CreateProcess
ffmpegSplitProc dir video = p{ cwd = Just dir
                             , std_in = NoStream
                             , close_fds = True
                             }
  where p = proc "ffmpeg" [ "-y", "-nostdin"
                          , "-rtsp_transport", "tcp"
                          , "-loglevel", "warning"
                          , "-i", video
                          , "-f", "segment"
                          , "-c", "copy"
                          , "tmp-%d.mp4"
                          ]
