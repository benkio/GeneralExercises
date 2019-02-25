module Pure.DomainSpec where

import Test.Hspec

spec :: Spec
spec = describe "Pure.Domain" $ do it "works" $ do True `shouldBe` True

main :: IO ()
main = hspec spec