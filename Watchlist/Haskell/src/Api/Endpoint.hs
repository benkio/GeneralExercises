{-# LANGUAGE DuplicateRecordFields #-}
module Api.Endpoint where

import Api.Domain
import Api.State
import Api.Request
import Api.Response
import Servant
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.STM
import Data.Bifunctor
import Data.List
import Control.Concurrent.STM.TVar

getContentEndpoint :: UserRequest -> AppM WatchListResponse
getContentEndpoint UserRequest{userId=usr} = do
  inputStore <- readStore
  inputUser <- validateUser usr
  result <- getUserContentAppM inputUser inputStore
  return $ WatchListResponse (getContent result)

addUserEndpoint :: UserRequest -> AppM NoContent
addUserEndpoint UserRequest{userId=usr} = do
  inputUser <- validateUser usr
  inputStore <- readStore
  let newStore = addUser inputUser inputStore
  _ <- writeStore newStore
  return NoContent

addContentEndpoint :: AddContentRequest -> AppM WatchListResponse
addContentEndpoint AddContentRequest{userId=usr, content=cs} = do
  inputUser <- validateUser usr
  inputStore <- readStore
  _ <- getUserContentAppM inputUser inputStore --404 check
  let newStore = addContent inputUser cs inputStore
  _ <- writeStore newStore
  result <- getUserContentAppM inputUser inputStore
  return $ WatchListResponse (getContent result)

deleteContentEndpoint :: DeleteContentRequest -> AppM WatchListResponse
deleteContentEndpoint DeleteContentRequest{userId=usr, content=cs} = do
  inputUser <- validateUser usr
  inputStore <- readStore
  _ <- getUserContentAppM inputUser inputStore --404 check
  let newStore = foldl' (flip (deleteContent inputUser)) inputStore cs
  _ <- writeStore newStore
  result <- getUserContentAppM inputUser inputStore
  return $ WatchListResponse (getContent result)

-- Common Logic ---------------------------------------
-- Probably need to move them to some validation package, but I'm too lazy

validateUser :: String -> AppM User
validateUser = liftEither . first (\_ -> err400) . createUser

getUserContentAppM :: User -> Store -> AppM WatchList
getUserContentAppM inputUser inputStore = do
  result <- liftEither (
    case (getUserContent inputUser inputStore) of
      Nothing -> Left err404
      Just x -> Right x
    )
  return result
