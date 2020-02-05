{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeApplications           #-}
module Api.DomainSpec where

import Refined
import Api.Domain
import Test.Hspec
import Data.Either

-- TestData -------------------------------------------------------------------
user :: User
user = User { userId = "abc" }

content :: ContentID
content = ContentID (fromRight undefined (refine @(SizeEqualTo 4) "1234"))

wl :: WatchList
wl = WatchList [content]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "deleteContent" $ do
    it "deletes existing content" $ pending
  describe "addContent" $ do
    it "adds new content for the user into the store " $ pending
  describe "createUser" $ do
    it "creates an user starting from it's id" $ pending
    it "fails if the input ids is not alphanumeric and of size 3" $ pending
