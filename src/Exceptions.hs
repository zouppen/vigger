module Exceptions where

import Control.Exception

data ViggerException = ViggerStop
                     | ViggerNonFatal String
                     | ViggerEncodeFail Int
                     deriving Show

instance Exception ViggerException

-- |Useful with try ... either to catch IOException only.
ioeConst :: a -> IOException -> a
ioeConst = const
