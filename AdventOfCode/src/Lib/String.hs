module Lib.String (bytestringToString) where

import qualified Data.ByteString as DBS
import Text.Printf (printf)

bytestringToString :: DBS.ByteString -> String
bytestringToString = concatMap (printf "%02x") . DBS.unpack
