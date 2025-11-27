module TwentySixteen.December09 where

input :: IO String
input = init <$> readFile "input/2016/9December.txt"

solution :: Bool -> Bool -> String -> Int
solution _ _ [] = 0
solution decompressVersion1 decompressVersion2 s@(x : xs)
    | x == '(' && decompressVersion1 =
        let (countDecompressed, rest) = decompressString decompressVersion2 s
         in countDecompressed + solution decompressVersion1 decompressVersion2 rest
    | otherwise = 1 + solution decompressVersion1 decompressVersion2 xs

decompressString :: Bool -> String -> (Int, String)
decompressString decompressVersionOne s =
    let (count, repetitions, s') = parseCompressPattern s
     in ( repetitions
            * solution decompressVersionOne decompressVersionOne (take count s')
        , drop count s'
        )

parseCompressPattern :: String -> (Int, Int, String)
parseCompressPattern s =
    ( ((\x -> read x :: Int) . takeWhile ('x' /=)) (tail s)
    , ((\x -> read x :: Int) . takeWhile (')' /=) . tail . dropWhile ('x' /=))
        (tail s)
    , (tail . dropWhile (')' /=)) s
    )

december09Solution1 :: IO Int
december09Solution1 = solution True False <$> input

december09Solution2 :: IO Int
december09Solution2 = solution True True <$> input
