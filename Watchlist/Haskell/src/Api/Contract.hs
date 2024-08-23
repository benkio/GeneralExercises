{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Contract where

import Api.Domain
import qualified Api.Endpoint as E
import Api.Request
import Api.Response
import Api.State
import Control.Monad.Reader
import Servant

type API =
    "v1" :> "content" :> ReqBody '[JSON] UserRequest :> Get '[JSON] WatchListResponse
        :<|> "v1" :> "content" :> ReqBody '[JSON] AddContentRequest :> Post '[JSON] WatchListResponse
        :<|> "v1" :> "content" :> ReqBody '[JSON] DeleteContentRequest :> Delete '[JSON] WatchListResponse
        :<|> "v1" :> "user" :> ReqBody '[JSON] UserRequest :> PostCreated '[JSON] NoContent

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = E.getContentEndpoint :<|> E.addContentEndpoint :<|> E.deleteContentEndpoint :<|> E.addUserEndpoint

app :: State -> Application
app s = serve api $ hoistServer api ((flip runReaderT) s) server
