{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Api.Domain where

import Refined
import qualified Data.List as L
import Data.Hashable
import Data.Char
import Data.Typeable
import Data.HashMap.Lazy
import Control.Monad

newtype ContentID = ContentID (Refined (SizeEqualTo 4) String) deriving (Eq)
newtype WatchList = WatchList [ContentID]
newtype UserId    = UserId String                                deriving (Eq, Hashable)
newtype User      = User { userId :: UserId }                    deriving (Eq, Hashable)
newtype Store     = Store (HashMap User WatchList)

instance Predicate UserId String where
  validate p x = do
    _ <- validate (SizeEqualTo 3) x
    _ <- unless (foldl' isAlpha True x) $ return () -- do throwRefineOtherException (typeOf p) ( "Not an alphanumeric string" )
    return ()

instance Semigroup WatchList where
  (WatchList wl) <> (WatchList wl') = WatchList (wl ++ wl')

instance Monoid WatchList where
  mempty = WatchList []

emptyStore :: Store
emptyStore = Store empty

createUser :: String -> Either RefineException UserId
createUser = refine

deleteFromWatchList :: ContentID -> WatchList -> WatchList
deleteFromWatchList c (WatchList l) = WatchList $ L.delete c l

addContent :: User -> ContentID -> Store -> Store
addContent user content (Store s) =
   Store $ insertWith (<>) user (WatchList [content]) s

deleteContent :: User -> ContentID -> Store -> Store
deleteContent user content (Store s) =
  Store $ adjust (deleteFromWatchList content) user s
