module Pure.Domain(
  CallLog(..),
  Call(..),
  number,
  parseDuration) where

import           Data.Hourglass
import           Data.List.Split
import           Data.Int
import           Data.Text (Text, pack)
import           Text.Regex
import           Text.Read

data CallLog = CallLog { clCostumerId :: String,
                         clCalled :: String,
                         clDuration :: String }

data Call = Call { cCostumerId :: String,
                   cCalled :: Number,
                   cDuration :: Duration }

type Number = Text

number :: String -> Maybe [Text]
number s = (fmap . fmap) pack (matchRegex (mkRegex "([0-9][0-9][0-9]-[0-9][0-9][0-9]-[0-9][0-9][0-9])") s)

-- String in format hh:mm:ss
parseDuration :: String -> Maybe Duration
parseDuration s = do
  let sp = splitOn ":" s
  stringParsed <- if (Prelude.length sp == 3) then Just sp else Nothing
  resultList <-  traverse (\s -> readMaybe s :: Maybe Int64) stringParsed
  return Duration {
     durationHours = Hours $ resultList !! 0       -- ^ number of hours
    , durationMinutes = Minutes $ resultList !! 1     -- ^ number of minutes
    , durationSeconds = Seconds $ resultList !! 2     -- ^ number of seconds
    , durationNs = NanoSeconds 0}