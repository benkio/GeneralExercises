module Pure.DomainSpec where

import Pure.Domain
import Test.Hspec

cardsValues :: [(CardValue, Int)]
cardsValues = [
  (Two, 2),
  (Three  , 3),
  (Four   , 4),
  (Five   , 5),
  (Six    , 6),
  (Seven  , 7),
  (Eight  , 8),
  (Nine   , 9),
  (Ten    , 10),
  (Jack   , 10),
  (King   , 10),
  (Queen  , 10),
  (Ace    , 11)
  ]

spec :: Spec
spec =
  describe "Domain" $ do
    describe "to21Value" $ do
      it "return the expected value" $
       mapM_ (\t -> to21Value (fst t) `shouldBe` (snd t) ) cardsValues
    describe "sam" $ do
      it "should have an empty hand" $
        (hand sam) `shouldBe` []
      it "should have a name as \"Sam\"" $
        (name sam) `shouldBe` "Sam"
    describe "dealer" $ do
      it "should have an empty hand" $
        (hand dealer) `shouldBe` []
      it "should have a name as \"Dealer\"" $
        (name dealer) `shouldBe` "Dealer"
    describe "deck" $ do
      it "should have 52 cards" $
        (length deck) `shouldBe` 52
      it "shold have 12 cards foreach card suit" $
        mapM_ (\ct -> length (filter ((== ct) . cType) deck) `shouldBe` 13) ((enumFrom (toEnum 0)) :: [CardType])
main :: IO ()
main = hspec spec