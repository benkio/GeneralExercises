{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Api.Contract where

import Servant
import Api.Domain
import Api.Request
import Api.Response

type API = "v1" :> "content" :> (
  ReqBody '[JSON] UserRequest          :> Get '[JSON] WatchListResponse    :<|>
  ReqBody '[JSON] AddContentRequest    :> Post '[JSON] WatchListResponse   :<|>
  ReqBody '[JSON] DeleteContentRequest :> Delete '[JSON] WatchListResponse :<|>
  ReqBody '[JSON] CreateUserRequest    :> PostCreated '[JSON] NoContent
  )


api :: Proxy API
api = Proxy

server :: Server API
server = undefined -- echoEndpoint :<|> sampleEndpoint

app :: Application
app = serve api server
