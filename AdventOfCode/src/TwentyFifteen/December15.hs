module TwentyFifteen.December15 where

import Data.List

data Ingredient = Ingredient
    { name :: String
    , capacity :: Int
    , durability :: Int
    , flavor :: Int
    , texture :: Int
    , calories :: Int
    }
    deriving (Show)

input :: IO [Ingredient]
input = fmap parseIngredient . lines <$> readFile "input/2015/15December.txt"

parseIngredient :: String -> Ingredient
parseIngredient =
    ( \s ->
        Ingredient
            { name = (init . head) s
            , capacity = read (init (s !! 2)) :: Int
            , durability = read (init (s !! 4)) :: Int
            , flavor = read (init (s !! 6)) :: Int
            , texture = read (init (s !! 8)) :: Int
            , calories = read (s !! 10) :: Int
            }
    )
        . words

inputTest :: [Ingredient]
inputTest =
    (fmap parseIngredient . lines)
        "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8\n\
        \Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"

ingredientTablespoon :: Ingredient -> Int -> [Int]
ingredientTablespoon
    Ingredient
        { capacity = ic
        , durability = id'
        , flavor = if'
        , texture = it
        , calories = ical
        }
    ts = fmap (ts *) [ic, id', if', it, ical]

tablespoonsPerIngredient :: [[Int]] -> Int -> [[Int]]
tablespoonsPerIngredient [] _ = [[]]
tablespoonsPerIngredient (xs : xss) upperBound =
    [ a : b
    | a <- xs
    , b <- tablespoonsPerIngredient xss (upperBound - a)
    , sum (a : b) == upperBound
    ]

solution1 :: [Ingredient] -> Int
solution1 is =
    ( maximum
        . fmap
            ( ( \l ->
                    if any (< 0) l
                        then 0
                        else product l
              )
                . fmap sum
                . transpose
                . fmap (init . uncurry ingredientTablespoon)
                . zip is
            )
    )
        $ tablespoonsPerIngredient (fmap (const [0 .. 100]) is) 100

solution1Test :: Bool
solution1Test = solution1 inputTest == 62842880

december15Solution1 :: IO Int
december15Solution1 = solution1 <$> input

solution2 :: [Ingredient] -> Int
solution2 is =
    ( maximum
        . fmap
            ( ( \l ->
                    if any (< 0) l
                        then 0
                        else product l
              )
                . fmap sum
                . init
            )
        . filter ((== 500) . sum . last)
        . fmap (transpose . fmap (uncurry ingredientTablespoon) . zip is)
    )
        $ tablespoonsPerIngredient (fmap (const [0 .. 100]) is) 100

solution2Test :: Bool
solution2Test = solution2 inputTest == 57600000

december15Solution2 :: IO Int
december15Solution2 = solution2 <$> input
