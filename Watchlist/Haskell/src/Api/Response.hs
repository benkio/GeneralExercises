{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Api.Response where

import Api.Domain
import GHC.Generics
import Data.Aeson

newtype WatchListResponse = WatchListResponse {
  watchlist :: [ContentID]
  } deriving (Generic, ToJSON)
