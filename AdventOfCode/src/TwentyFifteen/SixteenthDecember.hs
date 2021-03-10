{-# LANGUAGE TypeApplications #-}

module TwentyFifteen.SixteenthDecember where

import Data.List (isInfixOf)
import Data.Maybe
import Data.Text as Text (pack, splitOn, takeWhile, unpack)

data AuntSue = AuntSue
  { sue_id :: Int,
    sue_children :: Children,
    sue_cats :: Cat,
    sue_samoyed :: Samoyed,
    sue_pomeranian :: Pomeranian,
    sue_akita :: Akita,
    sue_vizsla :: Vizsla,
    sue_goldfish :: Goldfish,
    sue_tree :: Tree,
    sue_car :: Car,
    sue_perfume :: Perfume
  }
  deriving (Show)

class AuntSueAttribute a where
  parse :: String -> a
  value :: a -> Maybe Int

newtype Children
  = Children (Maybe Int)
  deriving (Show)

parseAuntAttribute ::
  (AuntSueAttribute a) => String -> String -> (Maybe Int -> a) -> a
parseAuntAttribute s key constructor =
  if key `isInfixOf` s
    then
      ( constructor
          . Just
          . (\x -> read x :: Int)
          . Text.unpack
          . Text.takeWhile (',' /=)
          . head
          . tail
          . Text.splitOn (Text.pack key)
          . Text.pack
      )
        s
    else constructor Nothing

instance AuntSueAttribute Children where
  parse s = parseAuntAttribute s "children: " Children
  value (Children x) = x

newtype Cat
  = Cat (Maybe Int)
  deriving (Show)

instance AuntSueAttribute Cat where
  parse s = parseAuntAttribute s "cats: " Cat
  value (Cat x) = x

newtype Samoyed
  = Samoyed (Maybe Int)
  deriving (Show)

instance AuntSueAttribute Samoyed where
  parse s = parseAuntAttribute s "samoyeds: " Samoyed
  value (Samoyed x) = x

newtype Pomeranian
  = Pomeranian (Maybe Int)
  deriving (Show)

instance AuntSueAttribute Pomeranian where
  parse s = parseAuntAttribute s "pomeranians: " Pomeranian
  value (Pomeranian x) = x

newtype Akita
  = Akita (Maybe Int)
  deriving (Show)

instance AuntSueAttribute Akita where
  parse s = parseAuntAttribute s "akitas: " Akita
  value (Akita x) = x

newtype Vizsla
  = Vizsla (Maybe Int)
  deriving (Show)

instance AuntSueAttribute Vizsla where
  parse s = parseAuntAttribute s "vizslas: " Vizsla
  value (Vizsla x) = x

newtype Goldfish
  = Goldfish (Maybe Int)
  deriving (Show)

instance AuntSueAttribute Goldfish where
  parse s = parseAuntAttribute s "goldfish: " Goldfish
  value (Goldfish x) = x

newtype Tree
  = Tree (Maybe Int)
  deriving (Show)

instance AuntSueAttribute Tree where
  parse s = parseAuntAttribute s "trees: " Tree
  value (Tree x) = x

newtype Car
  = Car (Maybe Int)
  deriving (Show)

instance AuntSueAttribute Car where
  parse s = parseAuntAttribute s "cars: " Car
  value (Car x) = x

newtype Perfume
  = Perfume (Maybe Int)
  deriving (Show)

instance AuntSueAttribute Perfume where
  parse s = parseAuntAttribute s "perfumes: " Perfume
  value (Perfume x) = x

targetAunt :: AuntSue
targetAunt =
  AuntSue
    { sue_id = 0,
      sue_children = Children (Just 3),
      sue_cats = Cat (Just 7),
      sue_samoyed = Samoyed (Just 2),
      sue_pomeranian = Pomeranian (Just 3),
      sue_akita = Akita (Just 0),
      sue_vizsla = Vizsla (Just 0),
      sue_goldfish = Goldfish (Just 5),
      sue_tree = Tree (Just 3),
      sue_car = Car (Just 2),
      sue_perfume = Perfume (Just 1)
    }

input :: IO [AuntSue]
input = fmap parseAunt . lines <$> readFile "input/2015/16December.txt"

parseAunt :: String -> AuntSue
parseAunt s =
  AuntSue
    { sue_id = (read @Int . Prelude.takeWhile (':' /=) . drop 4) s,
      sue_children = parse @Children s,
      sue_cats = parse @Cat s,
      sue_samoyed = parse @Samoyed s,
      sue_pomeranian = parse @Pomeranian s,
      sue_akita = parse @Akita s,
      sue_vizsla = parse @Vizsla s,
      sue_goldfish = parse @Goldfish s,
      sue_tree = parse @Tree s,
      sue_car = parse @Car s,
      sue_perfume = parse @Perfume s
    }

compareAuntSue :: AuntSue -> AuntSue -> Bool
compareAuntSue a a' =
  compareAuntSueAttributes (sue_children a) (sue_children a') (==)
    && compareAuntSueAttributes (sue_cats a) (sue_cats a') (==)
    && compareAuntSueAttributes (sue_samoyed a) (sue_samoyed a') (==)
    && compareAuntSueAttributes (sue_pomeranian a) (sue_pomeranian a') (==)
    && compareAuntSueAttributes (sue_akita a) (sue_akita a') (==)
    && compareAuntSueAttributes (sue_vizsla a) (sue_vizsla a') (==)
    && compareAuntSueAttributes (sue_goldfish a) (sue_goldfish a') (==)
    && compareAuntSueAttributes (sue_tree a) (sue_tree a') (==)
    && compareAuntSueAttributes (sue_car a) (sue_car a') (==)
    && compareAuntSueAttributes (sue_perfume a) (sue_perfume a') (==)

compareAuntSueAttributes ::
  (AuntSueAttribute a) => a -> a -> (Maybe Int -> Maybe Int -> Bool) -> Bool
compareAuntSueAttributes x y comparison
  | isJust (value x) && isJust (value y) = value x `comparison` value y
  | otherwise = True

solution1 :: [AuntSue] -> Int
solution1 = sue_id . head . filter (compareAuntSue targetAunt)

sixteenthDecemberSolution1 :: IO Int
sixteenthDecemberSolution1 = solution1 <$> input

compareAuntSueReal :: AuntSue -> AuntSue -> Bool
compareAuntSueReal a a' =
  compareAuntSueAttributes (sue_children a) (sue_children a') (==)
    && compareAuntSueAttributes (sue_cats a) (sue_cats a') (>)
    && compareAuntSueAttributes (sue_samoyed a) (sue_samoyed a') (==)
    && compareAuntSueAttributes (sue_pomeranian a) (sue_pomeranian a') (<)
    && compareAuntSueAttributes (sue_akita a) (sue_akita a') (==)
    && compareAuntSueAttributes (sue_vizsla a) (sue_vizsla a') (==)
    && compareAuntSueAttributes (sue_goldfish a) (sue_goldfish a') (<)
    && compareAuntSueAttributes (sue_tree a) (sue_tree a') (>)
    && compareAuntSueAttributes (sue_car a) (sue_car a') (==)
    && compareAuntSueAttributes (sue_perfume a) (sue_perfume a') (==)

solution2 :: [AuntSue] -> Int
solution2 = sue_id . head . filter (`compareAuntSueReal` targetAunt)

sixteenthDecemberSolution2 :: IO Int
sixteenthDecemberSolution2 = solution2 <$> input
