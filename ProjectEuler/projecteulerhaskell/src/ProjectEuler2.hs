{-# LANGUAGE TupleSections #-}
module ProjectEuler2 where

import Data.List (transpose, foldr, foldl', find, cycle, isPrefixOf, isSuffixOf)
import Data.Maybe (Maybe(..), fromJust)
import Data.Char (digitToInt)
import qualified Data.Set as S (Set, fromList)
import Control.Monad ((>>=))
import Control.Applicative ((<$>))


-- Es 11 ----------------------------------------------------------------------
inputGrid :: String
inputGrid =
  "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08 \n\
 \ 49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00 \n\
 \ 81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65 \n\
 \ 52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91 \n\
 \ 22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80 \n\
 \ 24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50 \n\
 \ 32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70 \n\
 \ 67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21 \n\
 \ 24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72 \n\
 \ 21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95 \n\
 \ 78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92 \n\
 \ 16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57 \n\
 \ 86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58 \n\
 \ 19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40 \n\
 \ 04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66 \n\
 \ 88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69 \n\
 \ 04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36 \n\
 \ 20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16 \n\
 \ 20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54 \n\
 \ 01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"

readIntLines :: String -> [[Int]]
readIntLines = (fmap (fmap (\x -> read x :: Int) . words) . lines)

horizontal :: [[Int]]
horizontal = readIntLines inputGrid

vertical :: [[Int]]
vertical = transpose horizontal

diagonal :: [[Int]] -> [[Int]]
diagonal grid =
  let indexXs = [0.. (((\x -> x - 1) . length . head) grid)]
      indexYs = [0.. (length grid - 1)]
      diagonalIndexSum = [0..((length grid * 2)
                              - 2)]
      indexes = [(x, y) | x <- indexXs, y <- indexYs]
      diagonalIndexes = [filter (\ip -> uncurry (+) ip == s) indexes | s <- diagonalIndexSum]
 in  (fmap . fmap) (\i -> (grid !! fst i) !! snd i) diagonalIndexes

diagonalSlash :: [[Int]]
diagonalSlash = diagonal horizontal

diagonalBackSlash :: [[Int]]
diagonalBackSlash = diagonal $ reverse horizontal

grouped :: Int -> [Int] -> [[Int]]
grouped _ [] = []
grouped n l = take n l : grouped n (drop n l)

maxGrid :: [[Int]] -> Int
maxGrid grid = maximum ( fmap (maximum . fmap product . grouped 4) grid)

maxVertical :: Int
maxVertical = maxGrid vertical

maxHorizontal :: Int
maxHorizontal = maxGrid horizontal

maxDiagonalSlash :: Int
maxDiagonalSlash = maxGrid diagonalSlash

maxDiagonalBackSlash :: Int
maxDiagonalBackSlash = maxGrid diagonalBackSlash

maxGrid' :: Int
maxGrid' = maximum [maxVertical, maxHorizontal, maxDiagonalSlash, maxDiagonalBackSlash]

-- Es 12 ------------------------------------------------------------

triangleNumbers :: [Int]
triangleNumbers = tail $ scanl (+) 0 [1..]

findDivisors :: Int -> S.Set Int
findDivisors n =
  let divisors = filter ((==0) . mod n) [1..((round . sqrt . fromIntegral) n)]
      divisors' = fmap (div n) divisors
  in  S.fromList $ divisors ++ divisors'

triangleNumberWith500Divisors :: Int
triangleNumberWith500Divisors = find' 500 triangleNumbers -- find from Data.List is not very efficient!!!
  where find' v xs
          | (length . findDivisors) (head xs) < v = find' v (tail xs)
          | otherwise = head xs

-- Es 13 -----------------------------------------------

inputGrid' :: String
inputGrid' =
  "37107287533902102798797998220837590246510135740250\n\
  \46376937677490009712648124896970078050417018260538\n\
  \74324986199524741059474233309513058123726617309629\n\
  \91942213363574161572522430563301811072406154908250\n\
  \23067588207539346171171980310421047513778063246676\n\
  \89261670696623633820136378418383684178734361726757\n\
  \28112879812849979408065481931592621691275889832738\n\
  \44274228917432520321923589422876796487670272189318\n\
  \47451445736001306439091167216856844588711603153276\n\
  \70386486105843025439939619828917593665686757934951\n\
  \62176457141856560629502157223196586755079324193331\n\
  \64906352462741904929101432445813822663347944758178\n\
  \92575867718337217661963751590579239728245598838407\n\
  \58203565325359399008402633568948830189458628227828\n\
  \80181199384826282014278194139940567587151170094390\n\
  \35398664372827112653829987240784473053190104293586\n\
  \86515506006295864861532075273371959191420517255829\n\
  \71693888707715466499115593487603532921714970056938\n\
  \54370070576826684624621495650076471787294438377604\n\
  \53282654108756828443191190634694037855217779295145\n\
  \36123272525000296071075082563815656710885258350721\n\
  \45876576172410976447339110607218265236877223636045\n\
  \17423706905851860660448207621209813287860733969412\n\
  \81142660418086830619328460811191061556940512689692\n\
  \51934325451728388641918047049293215058642563049483\n\
  \62467221648435076201727918039944693004732956340691\n\
  \15732444386908125794514089057706229429197107928209\n\
  \55037687525678773091862540744969844508330393682126\n\
  \18336384825330154686196124348767681297534375946515\n\
  \80386287592878490201521685554828717201219257766954\n\
  \78182833757993103614740356856449095527097864797581\n\
  \16726320100436897842553539920931837441497806860984\n\
  \48403098129077791799088218795327364475675590848030\n\
  \87086987551392711854517078544161852424320693150332\n\
  \59959406895756536782107074926966537676326235447210\n\
  \69793950679652694742597709739166693763042633987085\n\
  \41052684708299085211399427365734116182760315001271\n\
  \65378607361501080857009149939512557028198746004375\n\
  \35829035317434717326932123578154982629742552737307\n\
  \94953759765105305946966067683156574377167401875275\n\
  \88902802571733229619176668713819931811048770190271\n\
  \25267680276078003013678680992525463401061632866526\n\
  \36270218540497705585629946580636237993140746255962\n\
  \24074486908231174977792365466257246923322810917141\n\
  \91430288197103288597806669760892938638285025333403\n\
  \34413065578016127815921815005561868836468420090470\n\
  \23053081172816430487623791969842487255036638784583\n\
  \11487696932154902810424020138335124462181441773470\n\
  \63783299490636259666498587618221225225512486764533\n\
  \67720186971698544312419572409913959008952310058822\n\
  \95548255300263520781532296796249481641953868218774\n\
  \76085327132285723110424803456124867697064507995236\n\
  \37774242535411291684276865538926205024910326572967\n\
  \23701913275725675285653248258265463092207058596522\n\
  \29798860272258331913126375147341994889534765745501\n\
  \18495701454879288984856827726077713721403798879715\n\
  \38298203783031473527721580348144513491373226651381\n\
  \34829543829199918180278916522431027392251122869539\n\
  \40957953066405232632538044100059654939159879593635\n\
  \29746152185502371307642255121183693803580388584903\n\
  \41698116222072977186158236678424689157993532961922\n\
  \62467957194401269043877107275048102390895523597457\n\
  \23189706772547915061505504953922979530901129967519\n\
  \86188088225875314529584099251203829009407770775672\n\
  \11306739708304724483816533873502340845647058077308\n\
  \82959174767140363198008187129011875491310547126581\n\
  \97623331044818386269515456334926366572897563400500\n\
  \42846280183517070527831839425882145521227251250327\n\
  \55121603546981200581762165212827652751691296897789\n\
  \32238195734329339946437501907836945765883352399886\n\
  \75506164965184775180738168837861091527357929701337\n\
  \62177842752192623401942399639168044983993173312731\n\
  \32924185707147349566916674687634660915035914677504\n\
  \99518671430235219628894890102423325116913619626622\n\
  \73267460800591547471830798392868535206946944540724\n\
  \76841822524674417161514036427982273348055556214818\n\
  \97142617910342598647204516893989422179826088076852\n\
  \87783646182799346313767754307809363333018982642090\n\
  \10848802521674670883215120185883543223812876952786\n\
  \71329612474782464538636993009049310363619763878039\n\
  \62184073572399794223406235393808339651327408011116\n\
  \66627891981488087797941876876144230030984490851411\n\
  \60661826293682836764744779239180335110989069790714\n\
  \85786944089552990653640447425576083659976645795096\n\
  \66024396409905389607120198219976047599490197230297\n\
  \64913982680032973156037120041377903785566085089252\n\
  \16730939319872750275468906903707539413042652315011\n\
  \94809377245048795150954100921645863754710598436791\n\
  \78639167021187492431995700641917969777599028300699\n\
  \15368713711936614952811305876380278410754449733078\n\
  \40789923115535562561142322423255033685442488917353\n\
  \44889911501440648020369068063960672322193204149535\n\
  \41503128880339536053299340368006977710650566631954\n\
  \81234880673210146739058568557934581403627822703280\n\
  \82616570773948327592232845941706525094512325230608\n\
  \22918802058777319719839450180888072429661980811197\n\
  \77158542502016545090413245809786882778948721859617\n\
  \72107838435069186155435662884062257473692284509516\n\
  \20849603980134001723930671666823555245252804609722\n\
  \53503534226472524250874054075591789781264330331690"

first10DigitSum :: String
first10DigitSum = (take 10 . show . sum . fmap (\x -> read x :: Integer) . lines) inputGrid'

-- Es 14 ----------------------------------------------------

generator :: Int -> Int
generator n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise = 3 * n + 1

collatzSequence :: Int -> Int
collatzSequence n = countSequence n 0
  where
    countSequence :: Int -> Int -> Int
    countSequence 1 acc = acc + 1
    countSequence x acc = countSequence (generator x) (acc + 1)

largestCollatzSequence :: Int
largestCollatzSequence = fst $ foldr (\x acc -> let currentCollatz = collatzSequence x in
                                    if currentCollatz > snd acc
                                    then (x, currentCollatz) else acc
                               ) (1, 1) [1..1000000]

-- Es 15 ---------------------------------------------

-- https://mathworld.wolfram.com/LatticePath.html
-- Solution is the binomial coefficient (40 20) -> (40!/ 20!*20!)
latticePaths :: Integer
latticePaths =
  let numerator = foldl' (*) 1 [1..40]
      twentyFact = foldl' (*) 1 [1..20]
  in numerator `div` (twentyFact ^ (2 :: Integer))

-- Es 16 -----------------------------------------------------------------

powerDigitSum :: Int
powerDigitSum = (sum . fmap digitToInt. show)(2 ^ (1000 :: Integer))

-- Es 17 ------------------------------------------------------

oneDigitNumMap :: [(Int, String)]
oneDigitNumMap = [
  (0, ""), -- not used: eg ten, twenty...
  (1, "one"),
  (2, "two"),
  (3, "three"),
  (4, "four"),
  (5, "five"),
  (6, "six"),
  (7, "seven"),
  (8, "eight"),
  (9, "nine")]

twoDigitNumSpecial :: [(Int, String)]
twoDigitNumSpecial = [
  (10, "ten"),
  (11, "eleven"),
  (12, "twelve"),
  (13, "thirteen"),
  (14, "fourteen"),
  (15, "fifteen"),
  (16, "sixteen"),
  (17, "severteen"),
  (18, "eighteen"),
  (19, "nineteen"),
  (20, "twenty"),
  (30, "thirty"),
  (40, "forty"),
  (50, "fifty"),
  (80, "eighty")
  ]

numMapSimpleFind :: Int -> [(Int, String)] -> Maybe (Int, String)
numMapSimpleFind n = find (\x -> fst x == n)

twoDigitStringCount :: Int -> String
twoDigitStringCount n
  | n `elem` fmap fst oneDigitNumMap = (snd . fromJust . numMapSimpleFind n)  oneDigitNumMap
  | n `elem` fmap fst twoDigitNumSpecial = (snd . fromJust . numMapSimpleFind n) twoDigitNumSpecial
  | otherwise =
    let (dozens, singleDigit) = n `divMod` 10
        singleDigitCount = (snd . fromJust . numMapSimpleFind singleDigit) oneDigitNumMap
        standardDozen = (snd . fromJust . numMapSimpleFind dozens) oneDigitNumMap ++ "ty" ++ (if singleDigit == 0 then "" else "-")
        dozensStringCount = foldr (\x _ -> snd x ++ "-") standardDozen $ numMapSimpleFind (dozens * 10) twoDigitNumSpecial
    in dozensStringCount ++ singleDigitCount

threeDigitStringCount :: Int -> String
threeDigitStringCount n =
  let
  (hundreds, twoDigit) = n `divMod` 100
  hundredsStringCount = oneHundredCheck (hundreds, twoDigit)
  twoDigitCount = twoDigitStringCount twoDigit
  in hundredsStringCount ++ twoDigitCount
  where oneHundredCheck :: (Int, Int) -> String
        oneHundredCheck (h, h')
          | h /= 0 && h' == 0 = (snd . fromJust . numMapSimpleFind h) oneDigitNumMap ++ " hundred"
          | h /= 0 && h' /= 0 = (snd . fromJust . numMapSimpleFind h) oneDigitNumMap ++ " hundred and "
          | otherwise = ""

fourDigitStringCount :: Int -> String
fourDigitStringCount n =
  let
    (thousand, threeDigit) = n `divMod` 1000
    thousendStringCount = oneThousandCheck (thousand, threeDigit)
    twoDigitCount = threeDigitStringCount threeDigit
  in thousendStringCount ++ twoDigitCount
  where oneThousandCheck :: (Int, Int) -> String
        oneThousandCheck (t, t')
          | t /= 0 && (t' == 0) = (snd . fromJust . numMapSimpleFind t) oneDigitNumMap ++ " thousand"
          | t /= 0 && (t' >= 100) = (snd . fromJust . numMapSimpleFind t) oneDigitNumMap ++ " thousand "
          | t /= 0 && t' < 100 = (snd . fromJust . numMapSimpleFind t) oneDigitNumMap ++ " thousand and "
          | otherwise = ""

oneThousandNumbersToChar :: IO ()--[String]
oneThousandNumbersToChar = mapM_ (putStrLn . fourDigitStringCount) [1..1000]

oneThousandNumbersToCharCount :: Int
oneThousandNumbersToCharCount = sum $ fmap (length . filter (not . (`elem` " -")) . fourDigitStringCount) [1..1000]

-- Es 18 --------------------------------------

inputTriangle :: String
inputTriangle = "75                                           \n\
              \  95 64                                        \n\
              \  17 47 82                                     \n\
              \  18 35 87 10                                  \n\
              \  20 04 82 47 65                               \n\
              \  19 01 23 75 03 34                            \n\
              \  88 02 77 73 07 63 67                         \n\
              \  99 65 04 28 06 16 70 92                      \n\
              \  41 41 26 56 83 40 80 70 33                   \n\
              \  41 48 72 33 47 32 37 16 94 29                \n\
              \  53 71 44 65 25 43 91 52 97 51 14             \n\
              \  70 11 33 28 77 73 17 78 39 68 17 57          \n\
              \  91 71 52 38 17 14 91 43 58 50 27 29 48       \n\
              \  63 66 04 68 89 53 67 30 73 16 69 87 40 31    \n\
              \  04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"

triangle :: [[Int]]
triangle = readIntLines inputTriangle

maxPath :: Int
maxPath = sum $ foldr collapseTriangle (replicate 16 0) triangle
  where
    collapseTriangle :: [Int] -> [Int] -> [Int]
    collapseTriangle ts acc =
      let maxacc = (fmap (uncurry max). zip (tail acc)) acc
      in fmap (uncurry (+)) $ maxacc `zip` ts

-- Es 19 -------------------------------------------------------------

weekDays :: [String]
weekDays = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]

months :: [Int]
months = [1..12]

isLeap :: Int -> Bool
isLeap y
  | y `mod` 400 == 0                   = True
  | y `mod` 4 == 0 && y `mod` 100 /= 0 = True
  | otherwise                          = False

daysOfMonth :: Int -> Int -> [Int]
daysOfMonth y m
  | isLeap y && m == 2     = [1..29]
  | m == 2                 = [1..28]
  | m `elem` [9, 4, 6, 11] = [1..30]
  | otherwise              = [1..31]

years :: [Int]
years = [1900..2000]

calendar :: [String]
calendar =
  let mmyyyy = [(x, y) | y <- years, x <- months]
      ddmmyyyy = mmyyyy >>= \(x, y) -> (, x, y) <$> daysOfMonth y x
      d = take (length ddmmyyyy) $ cycle weekDays
      d_ddmmyyyy = fmap (\(x, (a, b, c)) ->
                           x ++ " " ++ show a ++ "/" ++ show b ++ "/" ++ show c) $ d `zip` ddmmyyyy
  in  d_ddmmyyyy

firstOfMonthSundays :: Int
firstOfMonthSundays =  (length . filter (\x -> isPrefixOf "Sunday 1/" x && (not . isSuffixOf "1900") x)) calendar

-- Es 20 -----------------------------------------------

factorialDigitSum :: Int
factorialDigitSum = (foldl' (\acc c -> acc + digitToInt c) 0 . show . foldl' (*) 1) [1..100]
