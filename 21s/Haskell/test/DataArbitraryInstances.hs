module DataArbitraryInstances where

import Pure.Domain
import Test.QuickCheck

instance Arbitrary CardValue where
  arbitrary = elements (enumFrom (toEnum 0))
instance Arbitrary CardType where
  arbitrary = elements (enumFrom (toEnum 0))
instance Arbitrary Card where
  arbitrary = do
    cv <- arbitrary :: Gen CardValue
    ct <- arbitrary :: Gen CardType
    return $ Card {cValue = cv, cType = ct}

instance Arbitrary Player where
  arbitrary = do
    handV <- listOf (arbitrary :: Gen Card)
    nameV <- listOf (arbitrary :: Gen Char)
    return (Player {hand=handV, name=nameV})