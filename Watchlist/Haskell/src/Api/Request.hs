{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Request where

import GHC.Generics
import Api.Domain
import Data.Aeson
import Data.Text

instance FromJSON ContentID where
  parseJSON (String content) = withText "Text" pure content <$> ContentID

newtype UserRequest = UserRequest { userId :: String }
data AddContentRequest = AddContentRequest {
  userId :: String ,
    content :: [ContentID]
  } deriving (Generic, FromJSON)
data DeleteContentRequest = DeleteContentRequest {
  userId :: String,
    content :: [ContentID]
  } deriving (Generic, FromJSON)
data CreateUserRequest = CreateUserRequest {
  userId :: String
  } deriving (Generic, FromJSON)
