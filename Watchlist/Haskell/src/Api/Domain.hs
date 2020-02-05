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
  createUser,
  createContent,
  emptyStore,
  Store(..),
  ContentID,
  WatchList(..),
  User) where

import qualified Data.List as L
import Refined
import Data.Hashable
import Data.Char
import Data.Typeable
import Data.HashMap.Lazy
import Control.Monad
import GHC.Generics (Generic)

newtype ContentID = ContentID String deriving (Eq)
newtype WatchList = WatchList [ContentID]
newtype User      = User { userId :: String }
  deriving (Eq, Hashable)
newtype Store     = Store (HashMap User WatchList)

data AlphanumericSizeThree = AlphanumericSizeThree
  deriving (Generic)

instance  Predicate AlphanumericSizeThree String where
  validate p x = do
    _ <- validate @(SizeEqualTo 3) undefined x
    _ <- unless (isAlphanumeric x) $ do
      throwRefine (RefineNotException (typeOf p))
    return ()

instance Semigroup WatchList where
  (WatchList wl) <> (WatchList wl') = WatchList (wl ++ wl')

instance Monoid WatchList where
  mempty = WatchList []

isAlphanumeric :: String -> Bool
isAlphanumeric = L.foldl' (\b c -> (isAlpha c) && b) True

emptyStore :: Store
emptyStore = Store empty

createUser :: String -> Either RefineException User
createUser s = fmap (User . unrefine) (refine @AlphanumericSizeThree @String s)

createContent :: String -> Either RefineException ContentID
createContent s = fmap (ContentID . unrefine) (refine @(SizeEqualTo 4) @String s)


deleteFromWatchList :: ContentID -> WatchList -> WatchList
deleteFromWatchList c (WatchList l) = WatchList $ L.delete c l

addContent :: User -> ContentID -> Store -> Store
addContent user content (Store s) =
   Store $ insertWith (<>) user (WatchList [content]) s

deleteContent :: User -> ContentID -> Store -> Store
deleteContent user content (Store s) =
  Store $ adjust (deleteFromWatchList content) user s
