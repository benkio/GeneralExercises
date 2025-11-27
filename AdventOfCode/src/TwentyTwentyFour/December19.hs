{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module TwentyTwentyFour.December19 where

import Data.Bifunctor (bimap, first)
import Data.IORef
import Data.List (drop, nub, stripPrefix, transpose)
import Data.Map (Map, adjust, empty, fromList, insert, keys, member, notMember, size, union, (!?))
import Data.Maybe (catMaybes, isJust, listToMaybe, mapMaybe)
import Data.Text (Text, pack, splitOn, takeWhile, unpack)
import Data.Text as Text (pack, splitOn, takeWhile, unpack)
import qualified Data.Text as T (lines)
import Data.Tree (Tree, drawTree, foldTree, unfoldForestM, unfoldTree)
import Lib.List (null')
import Text.ParserCombinators.ReadP (char, choice)
import Text.ParserCombinators.ReadPrec (lift)
import Text.Printf (printf)
import Text.Read (Read (..), read)

data Stripe = W | U | B | R | G deriving (Ord, Eq)

newtype Towel = T {p :: [Stripe]} deriving (Eq)

newtype Design = D {design :: [Stripe]} deriving (Eq, Ord)

instance Read Stripe where
    readPrec =
        lift
            ( choice
                [ W <$ char 'w'
                , U <$ char 'u'
                , B <$ char 'b'
                , R <$ char 'r'
                , G <$ char 'g'
                ]
            )

instance Show Stripe where
    show W = "w"
    show U = "u"
    show B = "b"
    show R = "r"
    show G = "g"

instance Show Towel where
    show T{p = stripes} = concatMap show stripes

instance Show Design where
    show D{design = stripes} = concatMap show stripes

input :: IO ([Towel], [Design])
input = parseInput <$> readFile "input/2024/December19.txt"

parseInput :: String -> ([Towel], [Design])
parseInput = (\[t, s] -> (parseTowels t, parseDesigns s)) . splitOn "\n\n" . pack
  where
    parseTowels :: Text -> [Towel]
    parseTowels = fmap (\x -> T{p = read . (: []) <$> unpack x}) . splitOn ", "
    parseDesigns :: Text -> [Design]
    parseDesigns = fmap (\x -> D{design = read . (: []) <$> unpack x}) . T.lines

suitableTowels :: [Towel] -> Design -> [(Towel, Design)]
suitableTowels ts (D{design = []}) = []
suitableTowels ts d =
    mapMaybe
        ( \t ->
            (\dStripes -> (t, D{design = read . (: []) <$> dStripes}))
                <$> stripPrefix (show t) (show d)
        )
        ts

towelsDesign :: Map Design [[Towel]] -> [Towel] -> Design -> ([[Towel]], Map Design [[Towel]])
towelsDesign kds ts (D{design = []}) = ([], kds)
towelsDesign kds ts d =
    (\(xs, k) -> (xs, updateKnowns d xs k)) $
        maybe (go kds d []) (,kds) $
            kds !? d
  where
    go :: Map Design [[Towel]] -> Design -> [[Towel]] -> ([[Towel]], Map Design [[Towel]])
    go knowns (D{design = []}) result = (result, knowns)
    go knowns d result =
        maybe (checkNewDesign result knowns d) (,knowns) $ knowns !? d

    checkNewDesign :: [[Towel]] -> Map Design [[Towel]] -> Design -> ([[Towel]], Map Design [[Towel]])
    checkNewDesign result knowns d =
        (\(xs, k) -> (xs, updateKnowns d xs k)) $
            foldl (newMatchFound result) ([], knowns) (suitableTowels ts d)

    newMatchFound :: [[Towel]] -> ([[Towel]], Map Design [[Towel]]) -> (Towel, Design) -> ([[Towel]], Map Design [[Towel]])
    newMatchFound result (acc, k) (t, dRest)
        | null (design dRest) = ([t] : acc, k)
        | otherwise = (result' ++ acc, k'')
      where
        (underSeq, k') = go k dRest result
        result' = fmap (t :) underSeq
        k'' = updateKnowns dRest underSeq k'

    updateKnowns = insert

isImpossibleDesign :: Map Design [[Towel]] -> [Towel] -> Design -> (Bool, Map Design [[Towel]])
isImpossibleDesign kds ts d = (null' combos, kds')
  where
    (combos, kds') = towelsDesign kds ts d

-- too low 314
solution1 :: ([Towel], [Design]) -> Int
solution1 input =
    length
        . filter not
        . fst
        . foldl
            ( \(acc, knowns) d ->
                let (result, knowns') = isImpossibleDesign knowns ts d
                 in (acc ++ [result], knowns')
            )
            ([], empty)
        $ ds
  where
    (ts, ds) = input

december19Solution1 :: IO Int
december19Solution1 = solution1 <$> input

solution2 :: ([Towel], [Design]) -> IO Int
solution2 input =
    sum . fmap countPossibilities <$> buildForest ts ds
  where
    (ts, ds) = input

buildForest :: [Towel] -> [Design] -> IO [Tree Int]
buildForest ts ds = do
    ms <- newIORef empty
    unfoldForestM (buildDesignForest ms ts) ds

buildDesignForest :: IORef (Map Design Int) -> [Towel] -> Design -> IO (Int, [Design])
buildDesignForest msRef ts d = do
    ms <- readIORef msRef
    maybe
        (unknownDesignF ms)
        (\i -> return (i, []))
        (ms !? d)
  where
    unknownDesignF knowns =
        let nextDesigns = fmap snd (suitableTowels ts d)
            childrenMapped = fmap (knowns !?) nextDesigns
            sumChildrens = sum . catMaybes $ childrenMapped
         in case (all isJust childrenMapped, null' childrenMapped, null (design d)) of
                (_, True, True) -> -- putStrLn ("No Children No Design " ++ show d) >> 
                    modifyIORef msRef (insert d 1) >> return (1, [])
                (_, True, _) -> -- putStrLn ("No Children with design " ++ show d) >> 
                    modifyIORef msRef (insert d 0) >> return (0, [])
                (True, _, _) ->
                    -- putStrLn ("from Map by Children " ++ show d ++ " sum " ++ show sumChildrens)
                    --     >>
                    modifyIORef msRef (insert d sumChildrens)
                        >> return (sumChildrens, [])
                (False, False, _) -> -- putStrLn ("Generate: " ++ show d) >> 
                    pure (0, nextDesigns)

countPossibilities :: Tree Int -> Int
countPossibilities = foldTree foldFunction
  where
    foldFunction v [] =
        v
    foldFunction _ xs =
        sum xs

test = fmap countPossibilities <$> buildForest ts ds
  where
    (ts, ds) = testInput
    d = ds !! 3

december19Solution2 :: IO Int
december19Solution2 = solution2 =<< input

testInput' :: ([Towel], [Design])
testInput' =
    parseInput
        "uurr, uugbw, rg, wugbbb, uru, ububw, uu, uwr, rgrgb, rurru, bbub, rww, urggbbb, rbur, grur, grw, guru, rgu, bwbw, ru, grrbbur, urr, bwbbbg, brrbr, wgw, rurbrbrr, wuu, wggw, wuuuru, wrg, ugww, gggrrg, gwruru, rrw, rbb, wgwrw, wug, bwurwbu, uurw, gbb, gbrbg, gwubwbrr, rbggr, bwgwg, uwg, guwbbw, rguwb, bgbu, grr, ubw, ggwbu, bburgr, u, urgb, rrb, wwbrru, ruruw, gr, uruwwur, brw, gwgu, rgbg, gug, rrbb, uuubub, rwuu, bru, uruuwbr, gubuuw, ruwu, rurbb, bu, bwbrwggr, wbw, ubwuw, buwugb, bur, uwwu, urb, rbrb, gu, wguwr, urrgw, ur, uwgbg, bbbugr, wuww, uggr, bbgwubb, uwgr, bubgb, bubu, bbr, rwrrb, gbuu, bgwbg, rrru, wgwrr, bug, bubrwb, burubu, bbwr, gwgg, uggubg, rgrub, ggbrrgu, wrbgru, gwr, uwgu, wuwub, gg, rrrbwrr, guw, gruu, rwrw, rbbgrb, rbugbr, buugbu, ggrrbuww, wgru, uur, wuwwr, bb, bgbg, gwurrwu, rbu, gwurg, rrrugwub, rgr, rru, wrgbgr, grg, rbrwrg, wrwgwwb, bugw, rwr, ugug, rwgg, ruw, ubg, brbw, bbrgrb, uww, uwwur, uubbr, wbrw, ggu, gbw, w, bggu, wwbrb, rbg, gbgubgu, bbbu, bgbur, uurur, uwuwu, uuwbw, wwgruub, wgwwwwr, buwr, gbuwg, wur, gbgrwb, buu, gbr, ggw, bwbbgbww, bruug, rruruu, bwgw, gww, ruwugggr, burb, brug, rbwurug, uuggwru, bbu, gbrgbuu, ruggwbw, wgur, gggru, rur, wuwgr, wwbwubr, guurwu, rwrrbub, wr, rugbgu, uugwwb, brr, gbwub, bub, rwrubw, uguwbggb, bruubbbw, wbrgu, ubb, buugr, bbwuww, wwugrw, wwb, bwgrwuw, wgg, bgwu, rguubw, uwrguw, uuwgu, ruwwb, ggg, urwub, rwu, bubuu, wubb, uwug, bg, gggbubg, rwrrwgu, wbr, brbu, bbuu, uruwur, uwwwrwbr, bwg, uub, grbrgw, bbug, wrgggrwg, gggurb, wgb, bbbbrbww, ggwb, rgw, bubuwgu, uwu, ubrurbu, grb, wrrw, wugw, brgwrugr, wrw, wgu, grgr, uuurw, ugrgrwr, www, bbg, ugg, gbu, uwrg, wbu, brb, rgb, rbrgbwu, ggb, wbwgr, grguwurr, brbrbr, ugw, gburg, gbbw, rrrbr, wwgg, rrr, bgu, wugbwgw, ruwuw, wwu, guug, gggub, gwbgw, wbwru, bwgru, rrugrw, uwuwrw, wbb, rr, gbuw, uwww, brg, urw, bgwb, wggggg, uwbbb, rwrr, rgwr, ugr, bgbggw, ubub, wrbggub, rrubr, ggrwr, wwur, wwuug, bbw, bbur, ruuwwrbr, ugrw, bwb, rrbwgw, bgbggwg, ubbw, uubwrg, rrwu, gbwb, bubg, brbggb, guugrb, wuwwru, rwrg, rgbrgr, uuw, ubrgwr, wwbrw, grbgg, urgg, bbrwu, rw, ubwwr, uugwg, ruubuw, wrrwrwuw, rbub, urg, buw, buuu, rbbwrr, rggwrw, urgugr, bwr, guu, wwuwug, rurb, bgb, rgbb, bgr, wuub, ubuwurw, buuwr, ggwuwgwb, gbug, ug, ruuru, rrg, rwuubww, wbg, buwb, ugrugb, gwb, wub, ggrbr, guwr, wwrwr, wgrw, bgw, ww, wuubgru, uw, bbwuurg, wuuwub, wrbuu, gw, bgrrgbb, wrbbgwwb, b, ruww, rbruu, bgg, uguww, wb, wgr, rubub, rbru, rrgb, ugwrww, gbg, wrgr, rbw, ugbuwww, gru, ruu, wugr, wgwg, wu, gwgbbug, wrbwrrgb, ugb, rwg, bgwburb, ggr, brggg, ruuuu, ugu, grbwu, ub, uwb, ubu, burwwrgr, wwg, gbwgu, wru, ggbwu, gwgubb, bbbww, rub, ubbbg, gwwb, rwugu, urggwbr, gwub, ubr, wrgu, gub, rb, wrb, rug, wuw, rubgrrg, bbbw, gbgrg, wbrggw, urbb, rbr, buuurg, uggu, bgrr, wrr, bubrg, urwu, wbbr, bww, gbbuug, rgg, bgbw, gb, uuwu, uuu, br, wgbg, rrbuurb, bwu, r, gur, rwug, uuwru, bbb, gugugw\n\
        \\n\
        \wbwbrugurbbuburgwruubgwurbgubgwuurbuuwuwrrrwrwgbw\n"

-- solution 2 2 1 4 6 0 1 2 0
testInput :: ([Towel], [Design])
testInput =
    parseInput
        "r, wr, b, g, bwu, rb, gb, br\n\
        \\n\
        \brwrr\n\
        \bggr\n\
        \gbbr\n\
        \rrbgbr\n\
        \ubwu\n\
        \bwurrg\n\
        \brgr\n\
        \bbrgwb\n"
