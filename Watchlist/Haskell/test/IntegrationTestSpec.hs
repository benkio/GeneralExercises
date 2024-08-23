module IntegrationTestSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Integration Test" $ do
        it "test something" $ pending
