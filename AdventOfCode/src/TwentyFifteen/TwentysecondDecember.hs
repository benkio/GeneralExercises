module TwentyFifteen.TwentysecondDecember where

import Control.Monad.State
import Control.Monad (when)
import System.Random

data Spell
    = MagicMissle
        { sMana :: Int
        }
    | Drain
        { sMana :: Int
        }
    | Shield
        { sMana :: Int
        }
    | Poison
        { sMana :: Int
        }
    | Recharge
        { sMana :: Int
        }
    deriving (Show, Eq)

data Boss = Boss
    { bHit :: Int
    , bDamage :: Int
    }
    deriving (Show)

data Player = Player
    { pHit :: Int
    , pArmor :: Int
    , pMana :: Int
    }
    deriving (Show)

player :: Player
player = Player{pHit = 50, pMana = 500, pArmor = 0}

input :: IO Boss
input = parseBoss <$> readFile "input/2015/22December.txt"

parseBoss :: String -> Boss
parseBoss s =
    let ls = lines s
        hits = ((\x -> read x :: Int) . drop 2 . dropWhile (':' /=) . head) ls
        damage = ((\x -> read x :: Int) . drop 2 . dropWhile (':' /=) . (!! 1)) ls
     in Boss{bHit = hits, bDamage = damage}

magicMissle :: Spell
magicMissle = MagicMissle{sMana = 53}

drain :: Spell
drain = Drain{sMana = 73}

shield :: Spell
shield = Shield{sMana = 113}

poison :: Spell
poison = Poison{sMana = 173}

recharge :: Spell
recharge = Recharge{sMana = 229}

spells :: [Spell]
spells = [magicMissle, drain, shield, poison, recharge]

nextAvailableSpells :: [Spell] -> [Spell]
nextAvailableSpells historySpell = filter (`notEffect` historySpell) spells
  where
    notEffect :: Spell -> [Spell] -> Bool
    notEffect s@Shield{} sp = s `notElem` drop (length sp - 2) sp
    notEffect s@Poison{} sp = s `notElem` drop (length sp - 2) sp
    notEffect s@Recharge{} sp = s `notElem` drop (length sp - 2) sp
    notEffect _ _ = True

instantSpell :: Spell -> Player -> Boss -> (Player, Boss)
instantSpell MagicMissle{} p b = (p, b{bHit = bHit b - 4})
instantSpell Drain{} p b = (p{pHit = pHit p + 2}, b{bHit = bHit b - 2})
instantSpell _ p b = (p, b)

fightBoss :: Spell -> [Spell] -> StateT (Player, Boss) IO [Spell]
fightBoss x history = do
    playerTurn x history
    let newHistory = history ++ [x]
    bossTurn newHistory
    (p, b) <- get
    let nextAvSp = nextAvailableSpells newHistory
    nextRandomSpell <- liftIO $ randomRIO (0, length nextAvSp - 1)
    if aliveCondition p b
        then fightBoss (nextAvSp !! nextRandomSpell) newHistory
        else return newHistory

playerTurn :: (Monad m) => Spell -> [Spell] -> StateT (Player, Boss) m ()
playerTurn x history = do
    applyEffects history True
    castSpell x

bossTurn :: (Monad m) => [Spell] -> StateT (Player, Boss) m ()
bossTurn history = do
    applyEffects history False
    bossAttack

applyEffects :: (Monad m) => [Spell] -> Bool -> StateT (Player, Boss) m ()
applyEffects xs isPlayerTurn = do
    (p, b) <- get
    let (p', b') = applyShield xs p b
    let (p'', b'') = applyPoison xs p' b'
    let (p''', b''') = applyRecharge xs p'' b'' isPlayerTurn
    put (p''', b''')

applyShield :: [Spell] -> Player -> Boss -> (Player, Boss)
applyShield sp p b
    | not (aliveCondition p b) = (p, b)
    | shield `elem` drop (length sp - 3) sp = (p{pArmor = 7}, b)
    | otherwise = (p{pArmor = 0}, b)

applyPoison :: [Spell] -> Player -> Boss -> (Player, Boss)
applyPoison sp p b
    | not (aliveCondition p b) = (p, b)
    | poison `elem` drop (length sp - 3) sp = (p, b{bHit = bHit b - 3})
    | otherwise = (p, b)

applyRecharge :: [Spell] -> Player -> Boss -> Bool -> (Player, Boss)
applyRecharge sp p b isPlayerTurn
    | not (aliveCondition p b) = (p, b)
    | recharge `elem` drop (length sp - 2) sp && isPlayerTurn =
        (p{pMana = pMana p + 101}, b)
    | recharge `elem` drop (length sp - 3) sp && not isPlayerTurn =
        (p{pMana = pMana p + 101}, b)
    | otherwise = (p, b)

castSpell :: (Monad m) => Spell -> StateT (Player, Boss) m ()
castSpell s = do
    (p, b) <- get
    when (aliveCondition p b) $ do
        let p' = p{pMana = pMana p - sMana s}
            (p'', b') = instantSpell s p' b
        put (p'', b')

bossAttack :: (Monad m) => StateT (Player, Boss) m ()
bossAttack = do
    (p, b) <- get
    when (aliveCondition p b) $ do
        let p' = p{pHit = pHit p - max 1 (bDamage b - pArmor p)}
        put (p', b)

aliveCondition :: Player -> Boss -> Bool
aliveCondition p b = pHit p > 0 && bHit b > 0 && pMana p > 0

calcManaSpent :: [Spell] -> Int
calcManaSpent = sum . fmap sMana

calcPotentialDamage :: [Spell] -> Int
calcPotentialDamage [] = 0
calcPotentialDamage (Drain{} : xs) = 2 + calcPotentialDamage xs
calcPotentialDamage (Poison{} : xs) = 18 + calcPotentialDamage xs
calcPotentialDamage (MagicMissle{} : xs) = 4 + calcPotentialDamage xs
calcPotentialDamage (_ : xs) = calcPotentialDamage xs

solution ::
    Boss ->
    Int ->
    Int ->
    (Spell -> [Spell] -> StateT (Player, Boss) IO [Spell]) ->
    IO Int
solution _ result 0 _ = return result
solution boss result count fBoss = do
    randomSpell <- randomRIO (0, length spells - 1)
    (newResult, (p, b)) <-
        runStateT (fBoss (spells !! randomSpell) []) (player, boss)
    if pHit p > 0 && bHit b <= 0 && pMana p > 0
        then solution boss (min (calcManaSpent newResult) result) (count - 1) fBoss
        else solution boss result count fBoss

twentysecondDecemberSolution1 :: IO Int
twentysecondDecemberSolution1 = do
    b <- input
    solution b 10000 100 fightBoss

twentysecondDecemberSolution2 :: IO Int
twentysecondDecemberSolution2 = do
    b <- input
    solution b 20000 1 fightBoss'

fightBoss' :: Spell -> [Spell] -> StateT (Player, Boss) IO [Spell]
fightBoss' x history = do
    playerTurn x history
    let newHistory = history ++ [x]
    bossTurn newHistory
    (p, b) <- get
    put (p{pHit = pHit p - 1}, b)
    let nextAvSp = nextAvailableSpells newHistory
    nextRandomSpell <- liftIO $ randomRIO (0, length nextAvSp - 1)
    if aliveCondition (p{pHit = pHit p - 1}) b
        then fightBoss' (nextAvSp !! nextRandomSpell) newHistory
        else return newHistory
