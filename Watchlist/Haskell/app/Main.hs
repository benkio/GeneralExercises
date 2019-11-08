module Main where

import Api.Implementation.Route
import Network.Wai.Handler.Warp

main :: IO ()
main = run 8080 app
