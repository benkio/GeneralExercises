{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Api.Contract where

import Servant
import Api.Domain

type API = "v1" :> "content" :> (
  ReqBody '[JSON] UserRequest          :> GET '[JSON] WatchListResponse    :<|>
  ReqBody '[JSON] AddContentRequest    :> POST '[JSON] WatchListResponse   :<|>
  ReqBody '[JSON] DeleteContentRequest :> DELETE '[JSON] WatchListResponse
  )


api :: Proxy API
api = Proxy
