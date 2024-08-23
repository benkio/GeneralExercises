{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Response where

import Api.Domain
import Data.Aeson
import GHC.Generics

newtype WatchListResponse = WatchListResponse
    { watchlist :: [ContentID]
    }
    deriving (Generic, ToJSON)
