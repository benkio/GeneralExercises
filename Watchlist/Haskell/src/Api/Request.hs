{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Api.Request where

import Api.Domain
import Data.Aeson
import GHC.Generics

newtype UserRequest = UserRequest {userId :: String} deriving (Generic, FromJSON)
data AddContentRequest = AddContentRequest
    { userId :: String
    , content :: [ContentID]
    }
    deriving (Generic, FromJSON)
data DeleteContentRequest = DeleteContentRequest
    { userId :: String
    , content :: [ContentID]
    }
    deriving (Generic, FromJSON)
