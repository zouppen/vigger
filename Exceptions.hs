module Exceptions where

import Control.Exception

data ViggerException = ViggerException String deriving Show

instance Exception ViggerException
