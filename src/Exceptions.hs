module Exceptions ( ViggerException(..)
                  , ioeConst
                  -- Re-exports
                  , try
                  , throw
                  , bracket
                  , viggerLoopCatch
                  ) where

import Control.Monad
import Control.Exception

data ViggerException = ViggerStop
                     | ViggerNonFatal String
                     | ViggerEncodeFail Int
                     deriving Show

instance Exception ViggerException

-- |Useful with try ... either to catch IOException only.
ioeConst :: a -> IOException -> a
ioeConst = const

-- |Loop until we receive relevant exception and determine what to do
-- then. Less fatal exceptions are just printed out but others cause a
-- graceful exit.
viggerLoopCatch :: IO a -> IO ()
viggerLoopCatch act = do
  continue <- catches (act >> pure True) [vigger, ctrlc]
  when continue $ viggerLoopCatch act
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
