module Api.Implementation.SampleEndpoint where

import Servant

sampleEndpoint :: Handler String
sampleEndpoint = return "Sample String"
