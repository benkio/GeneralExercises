{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeApplications           #-}
module Api.DomainSpec where

import Refined
import Api.Domain
import Test.Hspec
import Data.Either
import Data.Maybe
import qualified Data.HashMap.Lazy as HS

-- TestData -------------------------------------------------------------------
user :: User
user = fromRight (error "the test user should be valid") $ createUser "abc"

content :: ContentID
content = fromRight (error "the test content should be valid") $ createContent "1234"

wl :: WatchList
wl = WatchList [content]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "deleteContent" $ do
    it "deletes existing content" $ do
      let store = Store (HS.insert user wl HS.empty)
          resultstore = deleteContent user content store
        in resultstore == Store (HS.insert user (WatchList []) HS.empty)
  describe "addContent" $ do
    it "adds new content for the user into the store " $ do
      let store = addContent user [content] emptyStore
          expectedStore = Store (HS.insert user wl HS.empty)
        in store == expectedStore
  describe "getUserContent" $ do
    it "show the content of a user" $ do
      let store = Store (HS.insert user wl HS.empty)
          resultWatchlist = getUserContent user store
        in
        isJust resultWatchlist &&
        (case fromJust resultWatchlist of
           WatchList l -> l == [content])
  describe "createUser" $ do
    it "creates an user starting from it's id" $
      isRight $ createUser "abc"
    it "fails if the input ids is not alphanumeric" $
      isLeft $ createUser "ab#"
    it "fails if the input ids is not of size 3" $
      isLeft $ createUser "iiiiiiiii"
  describe "createContent"  $ do
    it "creates a content ID" $
      isRight $ createContent "abcd"
    it "fails if the input length is not equal to 4" $
      isLeft $ createContent "iiiiiiiii"
  describe "addUser" $ do
    it "adds a new user to the input store" $
      let (Store hs) = addUser user emptyStore
      in HS.member user hs
    it "ignore the input if the user is already present" $
      let
        initialStore = Store (HS.singleton user wl)
        result = addUser user initialStore
      in result == initialStore
