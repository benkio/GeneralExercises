{-# LANGUAGE DataKinds #-}
module Api.Domain.WatchlistItem where

import Refined

newtype ContentID = ContetID { value :: Refined (SizeEqualTo 4) String }
