module ThirdDecember where

input :: IO [(Int, String)]
input = do
  content <- readFile "input/3December.txt"
  return $ [0..] `zip` (cycle <$> lines content)

-- Right 3, down 1.
slopeCoordinates1 :: [(Int, Int)]
slopeCoordinates1 = [0..] `zip` [0, 3..]

-- Right 1, down 1.
slopeCoordinates2 :: [(Int, Int)]
slopeCoordinates2 = [0..] `zip` [0..]

-- Right 5, down 1.
slopeCoordinates3 :: [(Int, Int)]
slopeCoordinates3 = [0..] `zip` [0, 5..]

-- Right 7, down 1.
slopeCoordinates4 :: [(Int, Int)]
slopeCoordinates4 = [0..] `zip` [0, 7 ..]

-- Right 1, down 2.
slopeCoordinates5 :: [(Int, Int)]
slopeCoordinates5 = [0,2..] `zip` [0..]

treesEncountered :: [(Int, Int)] -> [(Int, String)] -> Int
treesEncountered ss =
  foldr (\l acc ->
            let mayYCordinate = last $ takeWhile ((fst l >=) . fst) ss
                mayYCordinate' = if fst mayYCordinate == fst l
                                 then Just (snd mayYCordinate)
                                 else Nothing
                block = maybe '.' (snd l !!) mayYCordinate'
            in if block == '#' then acc+1 else acc
        ) 0

thirdDecemberSolution1 :: IO Int
thirdDecemberSolution1 = treesEncountered slopeCoordinates1 <$> input

thirdDecemberSolution2 :: IO Int
thirdDecemberSolution2 =  (\i -> product (fmap (\f -> f i)
  [treesEncountered slopeCoordinates1
  ,treesEncountered slopeCoordinates2
  ,treesEncountered slopeCoordinates3
  ,treesEncountered slopeCoordinates4
  ,treesEncountered slopeCoordinates5])) <$> input
