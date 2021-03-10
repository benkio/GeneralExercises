module TwentyFifteen.TwentyfirstDecember where

import Data.List

data Gear = Gear
  { gName :: String,
    gCost :: Int,
    gDamage :: Int,
    gArmor :: Int
  }
  deriving (Show)

instance Semigroup Gear where
  (<>)
    Gear {gCost = gc1, gDamage = gd1, gArmor = ga1}
    Gear
      { gCost = gc2,
        gDamage = gd2,
        gArmor = ga2
      } =
      Gear
        { gName = "Gear Comp",
          gCost = gc1 + gc2,
          gDamage = gd1 + gd2,
          gArmor = ga1 + ga2
        }

data Boss = Boss
  { bHit :: Int,
    bDamage :: Int,
    bArmor :: Int
  }
  deriving (Show)

weapons :: [Gear]
weapons =
  [ Gear {gName = "Dagger", gCost = 8, gDamage = 4, gArmor = 0},
    Gear {gName = "Shortsword", gCost = 10, gDamage = 5, gArmor = 0},
    Gear {gName = "Warhammer", gCost = 25, gDamage = 6, gArmor = 0},
    Gear {gName = "Longsword", gCost = 40, gDamage = 7, gArmor = 0},
    Gear {gName = "Greataxe", gCost = 74, gDamage = 8, gArmor = 0}
  ]

armors :: [Gear]
armors =
  [ Gear {gName = "Leather", gCost = 13, gDamage = 0, gArmor = 1},
    Gear {gName = "Chainmail", gCost = 31, gDamage = 0, gArmor = 2},
    Gear {gName = "Splintmail", gCost = 53, gDamage = 0, gArmor = 3},
    Gear {gName = "Bandedmail", gCost = 75, gDamage = 0, gArmor = 4},
    Gear {gName = "Platemail", gCost = 102, gDamage = 0, gArmor = 5}
  ]

rings :: [Gear]
rings =
  [ Gear {gName = "Damage +1", gCost = 25, gDamage = 1, gArmor = 0},
    Gear {gName = "Damage +2", gCost = 50, gDamage = 2, gArmor = 0},
    Gear {gName = "Damage +3", gCost = 100, gDamage = 3, gArmor = 0},
    Gear {gName = "Defense +1", gCost = 20, gDamage = 0, gArmor = 1},
    Gear {gName = "Defense +2", gCost = 40, gDamage = 0, gArmor = 2},
    Gear {gName = "Defense +3", gCost = 80, gDamage = 0, gArmor = 3}
  ]

input :: IO Boss
input = parseBoss <$> readFile "input/2015/21December.txt"

parseBoss :: String -> Boss
parseBoss s =
  let ls = lines s
      hits = ((\x -> read x :: Int) . drop 2 . dropWhile (':' /=) . head) ls
      damage = ((\x -> read x :: Int) . drop 2 . dropWhile (':' /=) . (!! 1)) ls
      armor = ((\x -> read x :: Int) . drop 2 . dropWhile (':' /=) . (!! 2)) ls
   in Boss {bHit = hits, bDamage = damage, bArmor = armor}

gearCombinations :: [Gear]
gearCombinations = do
  w <- fmap (: []) weapons
  a <- [] : fmap (: []) armors
  r <- (filter ((<= 2) . length) . subsequences) rings
  let combination = w ++ a ++ r
  return $ foldl1 (<>) combination

winBoss :: Gear -> Int -> Boss -> Bool
winBoss g@Gear {gDamage = gd, gArmor = ga} playerHits b =
  let damageToPlayer = max 1 (bDamage b - ga)
      damageToBoss = max 1 (gd - bArmor b)
      bossHits = bHit b - damageToBoss
      playerHits' = playerHits - damageToPlayer
      boss' = b {bHit = bossHits}
   in case (bossHits <= 0, playerHits' <= 0) of
        (True, _) -> True
        (_, True) -> False
        _ -> winBoss g playerHits' boss'

inputTest :: String
inputTest = undefined

solution1 :: Boss -> Gear
solution1 b =
  ( minimumBy (\g g' -> compare (gCost g) (gCost g'))
      . filter (\g -> winBoss g 100 b)
  )
    gearCombinations

twentyfirstDecemberSolution1 :: IO Int
twentyfirstDecemberSolution1 = gCost . solution1 <$> input

solution2 :: Boss -> Gear
solution2 b =
  ( maximumBy (\g g' -> compare (gCost g) (gCost g'))
      . filter (\g -> not (winBoss g 100 b))
  )
    gearCombinations

twentyfirstDecemberSolution2 :: IO Int
twentyfirstDecemberSolution2 = gCost . solution2 <$> input
