module Api.Endpoint where

import Api.Domain
import Api.State
import Api.Request
import Api.Response
import Servant

getContent :: UserRequest -> AppM WatchListResponse
getContent = undefined

addUser :: UserRequest -> AppM NoContent
addUser = undefined

addContent :: AddContentRequest -> AppM WatchListResponse
addContent = undefined

deleteContent :: DeleteContentRequest -> AppM WatchListResponse
deleteContent = undefined
