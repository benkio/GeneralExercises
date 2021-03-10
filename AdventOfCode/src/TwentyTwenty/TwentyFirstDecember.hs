-------------------------------------------------------------------------------
--                           Advent Of Code - day 21                          --
-------------------------------------------------------------------------------
module TwentyTwenty.TwentyFirstDecember where

import Data.List (foldl1', intercalate, intersect)
import qualified Data.Map as M
  ( Map,
    difference,
    elems,
    empty,
    filter,
    insertWith,
    map,
    toList,
    union,
  )

type Ingredient = String

type Allergien = String

data Food
  = Food [Ingredient] [Allergien]
  deriving (Show)

input :: IO [Food]
input = fmap parseFood . lines <$> readFile "input/2020/21December.txt"

foodIngredients (Food x _) = x

foodAllergien (Food _ x) = x

parseFood :: String -> Food
parseFood =
  ( \(ing, allerg) ->
      Food ((filter ("" /=) . words) ing) ((fmap init . words . drop 10) allerg)
  )
    . span ('(' /=)

groupFoods :: [Food] -> M.Map Allergien [[Ingredient]]
groupFoods = foldl combineFoodsByAllergien M.empty
  where
    combineFoodsByAllergien ::
      M.Map Allergien [[Ingredient]] ->
      Food ->
      M.Map Allergien [[Ingredient]]
    combineFoodsByAllergien m (Food ing allerg) =
      foldl (\acc a -> M.insertWith (++) a [ing] acc) m allerg

intersectIngredientsInAllergen ::
  M.Map Allergien [[Ingredient]] -> M.Map Allergien [Ingredient]
intersectIngredientsInAllergen = M.map (foldl1' intersect)

commonIngredients :: M.Map Allergien [Ingredient] -> [Ingredient]
commonIngredients = concatMap snd . M.toList

twentyFirstDecemberSolution1 :: IO Int
twentyFirstDecemberSolution1 = do
  fs <- input
  let suspectIngredients =
        (commonIngredients . intersectIngredientsInAllergen . groupFoods) fs
      totalIngrediets = foldl (\acc f -> acc ++ foodIngredients f) [] fs
  return $ length (filter (`notElem` suspectIngredients) totalIngrediets)

reduceAllergens :: M.Map Allergien [Ingredient] -> M.Map Allergien [Ingredient]
reduceAllergens allIngr
  | null allIngr = M.empty
  | otherwise =
    dangerousIngredientMap `M.union` reduceAllergens restOfIngredients
  where
    dangerousIngredientMap :: M.Map Allergien [Ingredient]
    dangerousIngredientMap = M.filter ((1 ==) . length) allIngr
    restOfIngredients :: M.Map Allergien [Ingredient]
    restOfIngredients =
      M.map
        (filter (`notElem` (concat . M.elems) dangerousIngredientMap))
        allIngr
        `M.difference` dangerousIngredientMap

twentyFirstDecemberSolution2 :: IO String
twentyFirstDecemberSolution2 =
  intercalate ","
    . concatMap snd
    . M.toList
    . reduceAllergens
    . intersectIngredientsInAllergen
    . groupFoods
    <$> input
