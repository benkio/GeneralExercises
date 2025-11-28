module Lib.MD5 (generateMD5, generateMD5WithPrefix) where

import qualified Data.ByteString.Char8 as DBS
import qualified Crypto.Hash.MD5 as MD5
import Text.Printf (printf)

generateMD5 :: String -> String
generateMD5 = concatMap (printf "%02x") . DBS.unpack . MD5.hash . DBS.pack

generateMD5WithPrefix :: String -> String -> String
generateMD5WithPrefix p = generateMD5 . (p++)
