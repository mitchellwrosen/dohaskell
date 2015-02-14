{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
module SharedTypes where

import Prelude (Read)

import Data.Data
import Data.Text (Text)
import Fay.Yesod

#ifdef FAY
import FFI
#else
import Fay.FFI
#endif

data Command = RollDie (Returns Text)
    deriving (Read, Typeable, Data)
