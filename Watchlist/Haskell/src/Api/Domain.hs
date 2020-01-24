{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Api.Domain where

import Refined
import qualified Data.List as L
import Data.Hashable
import Data.Char
import Data.Typeable
import Data.HashMap.Lazy
import Control.Monad
import GHC.Generics (Generic)

newtype ContentID = ContentID (Refined (SizeEqualTo 4) String) deriving (Eq)
newtype WatchList = WatchList [ContentID]
newtype User      = User { userId :: (Refined AlphanumericSizeThree String) }
  deriving (Eq, Hashable)
newtype Store     = Store (HashMap User WatchList)

data AlphanumericSizeThree = AlphanumericSizeThree
  deriving (Generic)

instance Predicate AlphanumericSizeThree String where
  validate p x = do
    _ <- validate (SizeEqualTo 3) x
    _ <- unless (isAlphanumeric x) $ do
      throwRefineOtherException (typeOf p) $ "Value is not even."
    return ()

instance Semigroup WatchList where
  (WatchList wl) <> (WatchList wl') = WatchList (wl ++ wl')

instance Monoid WatchList where
  mempty = WatchList []

isAlphanumeric :: String -> Bool
isAlphanumeric = foldl' isAlpha True

emptyStore :: Store
emptyStore = Store empty

createUser :: String -> Either RefineException User
createUser = refine

deleteFromWatchList :: ContentID -> WatchList -> WatchList
deleteFromWatchList c (WatchList l) = WatchList $ L.delete c l

addContent :: User -> ContentID -> Store -> Store
addContent user content (Store s) =
   Store $ insertWith (<>) user (WatchList [content]) s

deleteContent :: User -> ContentID -> Store -> Store
deleteContent user content (Store s) =
  Store $ adjust (deleteFromWatchList content) user s
