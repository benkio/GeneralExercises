module TwentySixteen.TenthDecember where

import Data.Bifunctor
import Data.List
import Data.Map (Map, fromList)
import qualified Data.Map as Map (adjust, filter, foldr, map, union)
import Data.Maybe

data BotOuput
    = BotOutput Int
    | Output Int
    | NoOutput
    deriving (Show)

data Bot = Bot
    { key :: Int
    , value1 :: Maybe Int
    , value2 :: Maybe Int
    , lowOutput :: BotOuput
    , highOutput :: BotOuput
    }
    deriving (Show)

buildEmptyBot :: Int -> Int -> Bot
buildEmptyBot k v1 =
    Bot
        { key = k
        , value1 = Just v1
        , value2 = Nothing
        , lowOutput = NoOutput
        , highOutput = NoOutput
        }

buildEmptyBot' :: Int -> BotOuput -> BotOuput -> Bot
buildEmptyBot' k bo1 bo2 =
    Bot
        { key = k
        , value1 = Nothing
        , value2 = Nothing
        , lowOutput = bo1
        , highOutput = bo2
        }

input :: IO (Map Int Bot)
input =
    fromList . fmap (\x -> (key x, x)) . foldl parseBotsInstructions [] . lines
        <$> readFile "input/2016/10December.txt"

inputTest :: Map Int Bot
inputTest =
    (fromList . fmap (\x -> (key x, x)) . foldl parseBotsInstructions [] . lines)
        "value 5 goes to bot 2\n\
        \bot 2 gives low to bot 1 and high to bot 0\n\
        \value 3 goes to bot 1\n\
        \bot 1 gives low to output 1 and high to bot 0\n\
        \bot 0 gives low to output 2 and high to output 0\n\
        \value 2 goes to bot 2"

parseBotsInstructions :: [Bot] -> String -> [Bot]
parseBotsInstructions bs s
    | "value" `isPrefixOf` s = parseValueInstruction bs s
    | "bot" `isPrefixOf` s = parseGivenInstruction bs s
    | otherwise = error $ "Unexpected instruction: " ++ s

parseValueInstruction :: [Bot] -> String -> [Bot]
parseValueInstruction bs s =
    let (value, bot) =
            ((\l -> (read (l !! 1) :: Int, read (last l) :: Int)) . words) s
        targetBot =
            maybe
                (buildEmptyBot bot value)
                ( \b ->
                    if isJust (value1 b)
                        then b{value2 = Just value}
                        else b{value1 = Just value}
                )
                (find (\b -> key b == bot) bs)
     in targetBot : filter (\b -> key b /= bot) bs

parseGivenInstruction :: [Bot] -> String -> [Bot]
parseGivenInstruction bs s =
    let (bot, lOutValue, hOutValue) =
            ( ( \l ->
                    (read (l !! 1) :: Int, read (l !! 6) :: Int, read (last l) :: Int)
              )
                . words
            )
                s
        (lOut, hOut) =
            ( if (words s !! 5) == "output"
                then Output lOutValue
                else BotOutput lOutValue
            , if (words s !! 10) == "output"
                then Output hOutValue
                else BotOutput hOutValue
            )
        targetBot =
            maybe
                (buildEmptyBot' bot lOut hOut)
                (\b -> b{lowOutput = lOut, highOutput = hOut})
                (find (\b -> key b == bot) bs)
     in targetBot : filter (\b -> key b /= bot) bs

botMove :: Bot -> ([(Int, Int)], [(Int, Int)])
botMove
    Bot
        { value1 = Just v1
        , value2 = Just v2
        , lowOutput = BotOutput lbo
        , highOutput = BotOutput hbo
        } = ([(lbo, min v1 v2), (hbo, max v1 v2)], [])
botMove
    Bot
        { value1 = Just v1
        , value2 = Just v2
        , lowOutput = BotOutput lbo
        , highOutput = Output hbo
        } = ([(lbo, min v1 v2)], [(hbo, max v1 v2)])
botMove
    Bot
        { value1 = Just v1
        , value2 = Just v2
        , lowOutput = Output lbo
        , highOutput = BotOutput hbo
        } = ([(hbo, max v1 v2)], [(lbo, min v1 v2)])
botMove
    Bot
        { value1 = Just v1
        , value2 = Just v2
        , lowOutput = Output lbo
        , highOutput = Output hbo
        } = ([], [(lbo, min v1 v2), (hbo, max v1 v2)])
botMove _ = ([], [])

botsMove :: Map Int Bot -> (Map Int Bot, [(Int, Int)])
botsMove bs =
    let fullBots = Map.filter hasBothValues bs
        (newBots, outputs) =
            Map.foldr
                (\b (newBs, outs) -> bimap (newBs ++) (outs ++) (botMove b))
                ([], [])
                fullBots
        newBs' = foldl (\bs' (b, v) -> Map.adjust (valueToBot v) b bs') bs newBots
     in ( Map.union
            (Map.map (\x -> x{value1 = Nothing, value2 = Nothing}) fullBots)
            newBs'
        , outputs
        )

valueToBot :: Int -> Bot -> Bot
valueToBot v b@Bot{value1 = Nothing} = b{value1 = Just v}
valueToBot v b@Bot{value2 = Nothing} = b{value2 = Just v}
valueToBot v b =
    error $ "Impossible to assign a value: " ++ show v ++ " to bot " ++ show b

hasBothValues :: Bot -> Bool
hasBothValues Bot{value1 = Just _, value2 = Just _} = True
hasBothValues _ = False

computeOutputs :: Map Int Bot -> [(Int, Int)]
computeOutputs bs =
    ( snd
        . until
            (\(x, _) -> Map.foldr (\b y -> y && not (hasBothValues b)) True x)
            (\(bs', outs) -> ((\(bs'', outs') -> (bs'', outs' ++ outs)) . botsMove) bs')
    )
        (bs, [])

solution1 :: Map Int Bot -> (Int, Int) -> Int
solution1 bs (t1, t2) =
    ( key
        . fromJust
        . find endCondition
        . fst
        . until
            (\(x, _) -> any endCondition x)
            (\(bs', outs) -> ((\(bs'', outs') -> (bs'', outs' ++ outs)) . botsMove) bs')
    )
        (bs, [])
  where
    endCondition :: Bot -> Bool
    endCondition Bot{value1 = Just x, value2 = Just y} =
        null $ [x, y] \\ [t1, t2]
    endCondition _ = False

solution1Test :: Bool
solution1Test = solution1 inputTest (5, 2) == 2

tenthDecemberSolution1 :: IO Int
tenthDecemberSolution1 = (`solution1` (17, 61)) <$> input

solution2 :: Map Int Bot -> Int
solution2 =
    product . fmap snd . filter (\(x, _) -> x `elem` [0, 1, 2]) . computeOutputs

tenthDecemberSolution2 :: IO Int
tenthDecemberSolution2 = solution2 <$> input
