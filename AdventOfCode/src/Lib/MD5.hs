module Lib.MD5 (generateMD5, generateMD5WithPrefix) where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Lazy.Char8 as DBS
import Lib.String (bytestringToString)

generateMD5 :: String -> String
generateMD5 = bytestringToString . MD5.hashlazy . DBS.pack

generateMD5WithPrefix :: String -> String -> String
generateMD5WithPrefix p = generateMD5 . (p ++)
