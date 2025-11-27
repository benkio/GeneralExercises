{-# LANGUAGE GADTs #-}

module TestCase where

data TestCase where
    TestCase :: (Eq a, Show a) =>
        { testName :: String
        , testAction :: IO a
        , expectedResult :: a
        } -> TestCase


