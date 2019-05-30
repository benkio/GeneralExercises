module Pure.Domain where

import           Data.Time.Clock
import           Data.Text

data CallLog = CallLog { clCostumerId :: String,
                         clCalled :: String,
                         clDuration :: String }

data Call = Call { cCostumerId :: String,
                   cCalled :: Number,
                   cDuration :: NominalDiffTime }

type Number = Text

number :: String -> Maybe Number
number = undefined