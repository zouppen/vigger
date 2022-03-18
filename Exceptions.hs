module Exceptions where

import Control.Exception

data ViggerException = ViggerStop | ViggerNonFatal String deriving Show

instance Exception ViggerException
