{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TypeApplications          #-}
module ForthDecember where

import           Data.Char   (intToDigit)
import           Data.Either (rights)
import           Data.List   (find, isPrefixOf)
import           Data.Maybe  (fromJust, isJust)
import           Text.Printf (printf)
import           Text.Read   (readMaybe)

newtype HexColor = HexColor String deriving Show
data UnitOfMeasure = Centimeters Int | Inches Int | UnspecifiedMeasure Int deriving Show

data Passport = Passport {
  passportByr   :: BirthYear
  , passportIyr :: IssueYear
  , passportEyr :: ExpirationYear
  , passportHgt :: Height
  , passportHcl :: HairColor
  , passportEcl :: EyeColor
  , passportPid :: PassportID
  , passportCid :: CountryID
  } | NorthPoleCredentials {
    npcByr :: BirthYear
  , npcIyr :: IssueYear
  , npcEyr :: ExpirationYear
  , npcHgt :: Height
  , npcHcl :: HairColor
  , npcEcl :: EyeColor
  , npcPid :: PassportID
  } deriving Show

newtype BirthYear       = BirthYear Int deriving Show
newtype IssueYear       = IssueYear Int deriving Show
newtype ExpirationYear  = ExpirationYear Int deriving Show
newtype Height          = Height UnitOfMeasure deriving Show
newtype HairColor       = HairColor HexColor deriving Show
newtype EyeColor        = EyeColor String deriving Show
newtype PassportID      = PassportID Int
newtype CountryID       = CountryID Int deriving Show

instance Show PassportID where
  show (PassportID v) = printf "%09d" v

class ToValidate a where
  validate :: String -> Either String a

instance ToValidate HexColor where
  validate = hexColorValidation

instance ToValidate UnitOfMeasure where
  validate s = unitOfMeasureValidation s

instance ToValidate BirthYear where
  validate s = do
    let typeName = "BirthYear"
    year <- yearOrIdValidation s (getKey typeName) typeName id
    conditionRange year "BirthYear" (1920, 2002) BirthYear

instance ToValidate IssueYear where
  validate s = do
    let typeName = "IssueYear"
    year <- yearOrIdValidation s (getKey typeName) typeName id
    conditionRange year "IssueYear" (2010, 2020) IssueYear

instance ToValidate ExpirationYear where
  validate s = do
    let typeName = "ExpirationYear"
    year <- yearOrIdValidation s (getKey typeName) typeName id
    conditionRange year "ExpirationYear" (2020, 2030) ExpirationYear

instance ToValidate Height where
  validate s =
    let typeName = "Height"
    in Height <$> prefixValidation s (getKey typeName) typeName unitOfMeasureValidation

instance ToValidate HairColor where
  validate s =
     let typeName = "HairColor"
    in HairColor <$> prefixValidation s (getKey typeName) typeName hexColorValidation

instance ToValidate EyeColor where
  validate s = do
    let validEyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        conditionColorString x
          | x `elem` validEyeColors = Right $ EyeColor x
          | otherwise = Left $ s ++ " is not a valid value: " ++ show validEyeColors
    let typeName = "EyeColor"
    prefixValidation s (getKey typeName) typeName conditionColorString

instance ToValidate PassportID where
  validate s = do
    let typeName = "PassportId"
    value <- yearOrIdValidation s (getKey typeName) typeName id
    if (length s - 4) == 9 then Right (PassportID value) else Left $ s ++ " is not a valid passport Id"

instance ToValidate CountryID where
  validate s =
    let typeName = "CountryId"
    in yearOrIdValidation s (getKey typeName) typeName CountryID

instance ToValidate Passport where
  validate s = validatePasswordField (words s) <> validateNorthPoleCredentialsFields (words s)

keys :: [(String, String)]
keys = [("BirthYear"     , "byr")
       ,("IssueYear"     , "iyr")
       ,("ExpirationYear", "eyr")
       ,("Height"        , "hgt")
       ,("HairColor"     , "hcl")
       ,("EyeColor"      , "ecl")
       ,("PassportId"    , "pid")
       ,("CountryId"     , "cid")]

getKey :: String -> String
getKey s = (foldr (\x _ -> snd x) "" . find ((s ==) . fst)) keys

input :: IO [String]
input = do
  content <- readFile "input/4December.txt"
  return $ (concatInput . lines) content
  where concatInput :: [String] -> [String]
        concatInput [] = []
        concatInput xs = (unwords . takeWhile ("" /=))  xs : concatInput (safeTail (dropWhile ("" /=) xs))

safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs = tail xs

prefixValidation :: String -> String -> String -> (String -> Either String a) -> Either String a
prefixValidation s prefix typeName followUpValidation
    | (prefix ++ ":") `isPrefixOf` s = followUpValidation (drop (length prefix + 1) s)
    | otherwise = Left ("Unexpected " ++ typeName ++ " value: " ++ s)

yearOrIdValidation :: String -> String -> String -> (Int -> a) -> Either String a
yearOrIdValidation s prefix typeName typeConstructor =
  prefixValidation s prefix typeName (\x ->
                                        let yearOrId = readMaybe x :: Maybe Int
                                        in case yearOrId of
                                          Just v -> Right (typeConstructor v)
                                          Nothing -> Left ("Unexpected " ++ typeName ++ " value: " ++ x)
                                     )

unitOfMeasureValidation :: String -> Either String UnitOfMeasure
unitOfMeasureValidation s
  | (reverse . take 2 . reverse) s == "cm" && isJust measure = conditionRange (fromJust measure) "UnitOfMeasure" (150,193) Centimeters
  | (reverse . take 2 . reverse) s == "in" && isJust measure = conditionRange (fromJust measure) "UnitOfMeasure" (59, 76) Inches
  | otherwise = Left ("Unexpected UnitOfMeasure value: " ++ s)
  where measure = readMaybe (take (length s - 2) s) :: Maybe Int

hexColorValidation :: String -> Either String HexColor
hexColorValidation (x:xs)
  | x == '#' && length xs == 6 && all (\y -> y `elem` (['a'..'z'] ++ fmap intToDigit [0..9])) xs = Right (HexColor (x:xs))
  | otherwise = Left ("Unexpected HexColor value: " ++ (x:xs))

listValidation :: (ToValidate b) => String -> [String] -> Either String b
listValidation typeName xs = (foldr (\x _ -> validate x) (Left (typeName ++ " not found in input: " ++ show xs)) . find (isPrefixOf (getKey typeName))) xs

validateNorthPoleCredentialsFields :: [String] -> Either String Passport
validateNorthPoleCredentialsFields xs = do
  npcByrField <- listValidation @BirthYear      "BirthYear"      xs
  npcIyrField <- listValidation @IssueYear      "IssueYear"      xs
  npcEyrField <- listValidation @ExpirationYear "ExpirationYear" xs
  npcHgtField <- listValidation @Height         "Height"         xs
  npcHclField <- listValidation @HairColor      "HairColor"      xs
  npcEclField <- listValidation @EyeColor       "EyeColor"       xs
  npcPidField <- listValidation @PassportID     "PassportId"     xs
  return $ NorthPoleCredentials {
      npcByr = npcByrField
    , npcIyr = npcIyrField
    , npcEyr = npcEyrField
    , npcHgt = npcHgtField
    , npcHcl = npcHclField
    , npcEcl = npcEclField
    , npcPid = npcPidField
    }

validatePasswordField :: [String] -> Either String Passport
validatePasswordField xs = do
  npc <- validateNorthPoleCredentialsFields xs
  passportCidField <- listValidation @CountryID "CountryId"  xs
  return $ Passport {
      passportByr = npcByr npc
    , passportIyr = npcIyr npc
    , passportEyr = npcEyr npc
    , passportHgt = npcHgt npc
    , passportHcl = npcHcl npc
    , passportEcl = npcEcl npc
    , passportPid = npcPid npc
    , passportCid = passportCidField
    }

forthDecemberSolution1 :: IO Int
forthDecemberSolution1 = do
  inputData <- input
  let validatedPassports = fmap (validate @Passport) inputData
  (return . length . rights) validatedPassports

conditionRange :: Int -> String -> (Int, Int) -> (Int -> a) -> Either String a
conditionRange y typeName range constructor
  | y >= fst range && y <= snd range = Right $ constructor y
  | otherwise = Left $ show y ++ " invalid "++ typeName ++" range ("++(show . fst) range ++ "-" ++ (show . snd) range ++ ")"
