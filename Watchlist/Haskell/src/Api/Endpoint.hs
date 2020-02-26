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
import Data.Bifunctor
import Control.Concurrent.STM.TVar

getContentEndpoint :: UserRequest -> AppM WatchListResponse
getContentEndpoint UserRequest{userId=usr} = do
  state <- ask
  inputStore <- (liftIO . readTVarIO . store) state
  inputUser <- validateUser usr
  result <- liftEither
    (case (getUserContent inputUser inputStore) of
                          Nothing -> Left err404
                          Just x -> Right x)
  return $ WatchListResponse (getContent result)

addUserEndpoint :: UserRequest -> AppM NoContent
addUserEndpoint UserRequest{userId=usr} = do
  inputUser <- validateUser usr
  undefined


addContentEndpoint :: AddContentRequest -> AppM WatchListResponse
addContentEndpoint = undefined

deleteContentEndpoint :: DeleteContentRequest -> AppM WatchListResponse
deleteContentEndpoint = undefined

validateUser :: String -> AppM User
validateUser = liftEither . first (\_ -> err400) . createUser
