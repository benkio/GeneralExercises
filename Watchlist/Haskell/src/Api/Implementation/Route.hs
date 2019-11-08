module Api.Implementation.Route where

import Api.Contract
import Api.Implementation.EchoEndpoint
import Api.Implementation.SampleEndpoint
import Servant

server :: Server API
server = echoEndpoint :<|> sampleEndpoint

app :: Application
app = serve api server
