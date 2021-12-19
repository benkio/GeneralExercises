module TwentyTwentyOne.EighteenthDecember where

import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.List (delete)
import Data.Maybe (fromMaybe, isJust)
import Text.Read (readMaybe)

data SFN = SFNNode SFN SFN | SFNLeaf Int deriving (Eq)

instance Show SFN where
  show (SFNNode l r) = "[" ++ show l ++ "," ++ show r ++ "]"
  show (SFNLeaf v) = show v

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

sfnIsNode :: SFN -> Bool
sfnIsNode (SFNNode _ _) = True
sfnIsNode _ = False

sfnIsLeaf :: SFN -> Bool
sfnIsLeaf (SFNLeaf _) = True
sfnIsLeaf _ = False

sfnGetRight :: SFN -> Either Int SFN
sfnGetRight (SFNNode _ r) = Right r
sfnGetRight (SFNLeaf l) = Left l

sfnGetLeft :: SFN -> Either Int SFN
sfnGetLeft (SFNNode l _) = Right l
sfnGetLeft (SFNLeaf l) = Left l

sfnGetLeftR :: SFN -> Int
sfnGetLeftR (SFNNode l _) = sfnGetLeftR l
sfnGetLeftR (SFNLeaf l) = l

sfnGetRightR :: SFN -> Int
sfnGetRightR (SFNNode _ r) = sfnGetRightR r
sfnGetRightR (SFNLeaf l) = l

parseSFN :: String -> (SFN, String)
parseSFN [] = error "Cannot parse empty string"
parseSFN ('[' : xs) =
  let (left, rest) = parseSFN xs
      (right, rest') = parseSFN (dropWhile (== ',') rest)
   in (SFNNode left right, dropWhile (== ']') rest')
parseSFN xs
  | isJust (readMaybe x :: Maybe Int) = (SFNLeaf (read x :: Int), drop (length x) xs)
  | otherwise = error $ "Character " ++ x ++ " not recognized"
  where
    x = takeWhile (\a -> a /= ',' && a /= ']') xs

parseInput :: String -> [SFN]
parseInput = fmap (fst . parseSFN) . lines

addSFN :: SFN -> SFN -> SFN
addSFN s s' = let newSFN = SFNNode s s' in reduceSFN newSFN

unsafeSfnGetRight :: SFN -> SFN
unsafeSfnGetRight = fromRight (error "Unreachable") . sfnGetRight

unsafeSfnGetLeft :: SFN -> SFN
unsafeSfnGetLeft = fromRight (error "Unreachable") . sfnGetLeft

flattenSFN :: SFN -> Maybe (SFN, Maybe (Either Int Int))
flattenSFN (SFNNode l r)
  | sfnIsNode l = Just (SFNNode (SFNLeaf 0) (sfnAddGetLeft' (sfnGetRightR l) r), (Just . Left) (sfnGetLeftR l))
  | sfnIsNode r = Just (SFNNode (sfnAddGetRight' (sfnGetLeftR r) l) (SFNLeaf 0), (Just . Right) (sfnGetRightR r))
  | otherwise = Nothing
flattenSFN _ = Nothing

is1LevelSFN :: SFN -> Bool
is1LevelSFN (SFNNode (SFNLeaf _) (SFNNode (SFNLeaf _) (SFNLeaf _))) = True
is1LevelSFN (SFNNode (SFNNode (SFNLeaf _) (SFNLeaf _)) (SFNLeaf _)) = True
is1LevelSFN (SFNNode (SFNNode (SFNLeaf _) (SFNLeaf _)) (SFNNode (SFNLeaf _) (SFNLeaf _))) = True
is1LevelSFN _ = False

explode :: SFN -> SFN
explode s = fst $ fromMaybe (s, Nothing) $ intExplode 0 s
  where
    intExplode :: Int -> SFN -> Maybe (SFN, Maybe (Either Int Int))
    intExplode c sf
      | c >= 3 && is1LevelSFN sf = flattenSFN sf
      | otherwise =
        let nestedLeft = eitherToMaybe (sfnGetLeft sf) >>= intExplode (c + 1)
            nestedRight = eitherToMaybe (sfnGetRight sf) >>= intExplode (c + 1)
         in if isJust nestedLeft
              then nestedLeft <&> mergeSFNLeft sf
              else nestedRight <&> mergeSFNRight sf

mergeSFNLeft :: SFN -> (SFN, Maybe (Either Int Int)) -> (SFN, Maybe (Either Int Int))
mergeSFNLeft (SFNNode _ r) (sfl, Just (Right v)) = (SFNNode sfl (sfnAddGetLeft' v r), Nothing)
mergeSFNLeft (SFNNode _ r) (sfl, mev) = (SFNNode sfl r, mev)

mergeSFNRight :: SFN -> (SFN, Maybe (Either Int Int)) -> (SFN, Maybe (Either Int Int))
mergeSFNRight (SFNNode l _) (sfr, Just (Left v)) = (SFNNode (sfnAddGetRight' v l) sfr, Nothing)
mergeSFNRight (SFNNode l _) (sfr, mev) = (SFNNode l sfr, mev)

sfnAddGetLeft' :: Int -> SFN -> SFN
sfnAddGetLeft' v (SFNNode l r) = SFNNode (sfnAddGetLeft' v l) r
sfnAddGetLeft' v (SFNLeaf v') = SFNLeaf (v + v')

sfnAddGetRight' :: Int -> SFN -> SFN
sfnAddGetRight' v (SFNNode l r) = SFNNode l (sfnAddGetRight' v r)
sfnAddGetRight' v (SFNLeaf v') = SFNLeaf (v + v')

split :: SFN -> SFN
split (SFNLeaf v)
  | v >= 10 && even v = SFNNode (SFNLeaf (v `div` 2)) (SFNLeaf (v `div` 2))
  | v >= 10 && odd v = SFNNode (SFNLeaf (v `div` 2)) (SFNLeaf (1 + (v `div` 2)))
  | otherwise = SFNLeaf v
split (SFNNode l r) =
  let splitLeft = split l
   in if splitLeft == l then SFNNode l (split r) else SFNNode splitLeft r

explode' :: SFN -> SFN
explode' s = let s' = explode s in if s == s' then s' else explode' s'

reduceSFN :: SFN -> SFN
reduceSFN s = applyTraformations s $ cycle [explode', split]
  where
    applyTraformations :: SFN -> [SFN -> SFN] -> SFN
    applyTraformations s (t : t' : ts) =
      let s' = t s
          s'' = t' s'
       in if s == s'' then s else applyTraformations s'' ts

sumSFNs :: [SFN] -> SFN
sumSFNs = foldl1 addSFN

sfnMagnitude :: SFN -> Int
sfnMagnitude (SFNNode l r) = 3 * sfnMagnitude l + (2 * sfnMagnitude r)
sfnMagnitude (SFNLeaf v) = v

solution1 :: String -> Int
solution1 = sfnMagnitude . sumSFNs . parseInput

input :: IO String
input = readFile "input/2021/18December.txt"

inputTest :: String
inputTest =
  "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]\n\
  \[[[5,[2,8]],4],[5,[[9,9],0]]]\n\
  \[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]\n\
  \[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]\n\
  \[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]\n\
  \[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]\n\
  \[[[[5,4],[7,7]],8],[[8,3],8]]\n\
  \[[9,3],[[9,9],[6,[4,9]]]]\n\
  \[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]\n\
  \[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"

eighteenthDecemberSolution1 :: IO Int
eighteenthDecemberSolution1 = solution1 <$> input

solution2 :: String -> Int
solution2 s =
  let sfns = parseInput s
      additionsMagnitudes = [m | x <- sfns, y <- delete x sfns, m <- [sfnMagnitude (addSFN x y), sfnMagnitude (addSFN y x)]]
   in maximum additionsMagnitudes

eighteenthDecemberSolution2 :: IO Int
eighteenthDecemberSolution2 = solution2 <$> input
