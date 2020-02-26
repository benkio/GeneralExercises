module Api.Response where

import Api.Domain

newtype WatchListResponse = WatchListResponse { watchlist :: [ContentID] }
