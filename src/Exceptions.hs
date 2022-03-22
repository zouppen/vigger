module Exceptions where

import Control.Exception

data ViggerException = ViggerStop
                     | ViggerNonFatal String
                     | ViggerEncodeFail Int
                     deriving Show

instance Exception ViggerException
