module TwentySixteen.SixteenthDecember where

input :: IO String
input = init <$> readFile "input/2016/16December.txt"

generateDragonCurveStep :: String -> String
generateDragonCurveStep s =
  let s' = (fmap (\c -> if c == '0' then '1' else '0') . reverse) s
   in s ++ ['0'] ++ s'

generateDragonCurve :: Int -> String -> String
generateDragonCurve i s
  | length s >= i = take i s
  | otherwise = generateDragonCurve i (generateDragonCurveStep s)

generateChecksum :: String -> String
generateChecksum s
  | odd (length s) = s
  | otherwise = generateChecksum $ (fmap (\l -> if head l == last l then '1' else '0') . pairs) s

pairs :: String -> [String]
pairs [] = []
pairs s = take 2 s : pairs (drop 2 s)

solution :: Int -> String -> String
solution diskSize = generateChecksum . generateDragonCurve diskSize

sixteenthDecemberSolution1 :: IO String
sixteenthDecemberSolution1 = solution 272 <$> input

sixteenthDecemberSolution2 :: IO String
sixteenthDecemberSolution2 = solution 35651584 <$> input
