{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Request where

import GHC.Generics
import Data.Aeson
import Api.Domain

newtype UserRequest = UserRequest { userId :: String } deriving (Generic, FromJSON)
data AddContentRequest = AddContentRequest {
  userId :: String ,
    content :: [ContentID]
  } deriving (Generic, FromJSON)
data DeleteContentRequest = DeleteContentRequest {
  userId :: String,
    content :: [ContentID]
  } deriving (Generic, FromJSON)
