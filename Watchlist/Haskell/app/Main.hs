module Main where

import Api.Contract
import Api.State
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  initialState <- newEmptyState
  run 8080 (app initialState)
