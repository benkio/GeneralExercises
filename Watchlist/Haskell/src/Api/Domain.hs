{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Api.Domain(
  deleteContent,
  addContent,
  getContent,
  getUserContent,
  createUser,
  createContent,
  emptyStore,
  Store(..),
  ContentID(..),
  WatchList(..),
  User) where

import qualified Data.List as L
import Refined
import Data.Hashable
import Data.Char
import Data.Text
import Data.Typeable
import qualified Data.HashMap.Lazy as HS
import Control.Monad
import GHC.Generics (Generic)
import Data.Aeson

newtype ContentID = ContentID Text deriving (Eq)
newtype WatchList = WatchList [ContentID] deriving (Eq)
newtype User      = User { userId :: Text }
  deriving (Eq, Hashable)
newtype Store     = Store (HS.HashMap User WatchList) deriving (Eq)

data AlphanumericSizeThree = AlphanumericSizeThree
  deriving (Generic)

-- Instances ------------------------------------------------------------------

instance  Predicate AlphanumericSizeThree Text where
  validate p x = do
    _ <- validate @(SizeEqualTo 3) undefined (unpack x)
    _ <- unless (isAlphanumeric x) $ do
      throwRefine (RefineNotException (typeOf p))
    return ()

instance Semigroup WatchList where
  (WatchList wl) <> (WatchList wl') = WatchList (wl ++ wl')

instance Monoid WatchList where
  mempty = WatchList []

instance FromJSON ContentID where
  parseJSON (String content) = withText "Text" (\s -> return (ContentID s)) (String content)

instance ToJSON ContentID where
  toJSON (ContentID content) = toJSON content


-- Functions & Constructors ----------------------------------------

isAlphanumeric :: Text -> Bool
isAlphanumeric = L.foldl' (\b c -> (isAlpha c) && b) True . unpack

emptyStore :: Store
emptyStore = Store HS.empty

createUser :: String -> Either RefineException User
createUser s = fmap (User . unrefine) (refine @AlphanumericSizeThree @Text (pack s))

createContent :: String -> Either RefineException ContentID
createContent s = fmap (ContentID . pack . unrefine) (refine @(SizeEqualTo 4) @String s)

deleteFromWatchList :: ContentID -> WatchList -> WatchList
deleteFromWatchList c (WatchList l) = WatchList $ L.delete c l

addContent :: User -> ContentID -> Store -> Store
addContent user content (Store s) =
   Store $ HS.insertWith (<>) user (WatchList [content]) s

deleteContent :: User -> ContentID -> Store -> Store
deleteContent user content (Store s) =
  Store $ HS.adjust (deleteFromWatchList content) user s

getUserContent :: User -> Store -> Maybe WatchList
getUserContent user (Store hm) = HS.lookup user hm

getContent :: WatchList -> [ContentID]
getContent (WatchList c) = c
