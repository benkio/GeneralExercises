{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Api.Contract where

import Servant

type API =
  "echo" :> ReqBody '[PlainText] String :> Post '[PlainText] String :<|>
  "sample" :> Get '[PlainText] String

api :: Proxy API
api = Proxy
