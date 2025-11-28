module Main where

import TwentyFifteen.December06

main :: IO ()
main = do
    result <- december06Solution2
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: 14110788"
    if result == 14110788
        then putStrLn "✓ Test PASSED!"
        else putStrLn $ "✗ Test FAILED! Difference: " ++ show (result - 14110788)
