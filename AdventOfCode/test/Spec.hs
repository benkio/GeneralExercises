module Main where

import Test.Tasty (TestTree, defaultMain, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import TestCase (TestCase (..))
import qualified TwentyFifteen.Tests as T2015
import qualified TwentySixteen.Tests as T2016
import qualified TwentyTwenty.Tests as T2020
import qualified TwentyTwentyFive.Tests as T2025
import qualified TwentyTwentyFour.Tests as T2024
import qualified TwentyTwentyOne.Tests as T2021
import qualified TwentyTwentyThree.Tests as T2023
import qualified TwentyTwentyTwo.Tests as T2022

runTestWithTimeout :: TestCase -> TestTree
runTestWithTimeout (TestCase name testAction expected) =
    localOption (mkTimeout 30000000) $
        testCase name $ do
            actualResult <- testAction
            assertEqual ("Expected " ++ show expected) expected actualResult

main :: IO ()
main =
    defaultMain $
        testGroup
            "AdventOfCode Tests"
            [ testGroup "TwentyFifteen" $ fmap runTestWithTimeout T2015.tests
            , testGroup "TwentySixteen" $ fmap runTestWithTimeout T2016.tests
            , testGroup "TwentyTwenty" $ fmap runTestWithTimeout T2020.tests
            , testGroup "TwentyTwentyOne" $ fmap runTestWithTimeout T2021.tests
            , testGroup "TwentyTwentyTwo" $ fmap runTestWithTimeout T2022.tests
            , testGroup "TwentyTwentyThree" $ fmap runTestWithTimeout T2023.tests
            , testGroup "TwentyTwentyFour" $ fmap runTestWithTimeout T2024.tests
            , testGroup "TwentyTwentyFive" $ fmap runTestWithTimeout T2025.tests
            ]
