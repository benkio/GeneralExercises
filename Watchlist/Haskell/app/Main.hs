module Main where

import Api.Contract
import Network.Wai.Handler.Warp

main :: IO ()
main = run 8080 app
