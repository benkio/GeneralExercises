{-# LANGUAGE DataKinds #-}
module Api.Domain.WatchlistItem where

import Refined

type ContentID = Refined (SizeEqualTo 4) String
