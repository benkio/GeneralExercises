module Pure.Transformation where

import Pure.Domain

validateInput :: CallLog -> Maybe Call
validateInput cl = do
  called <- number (clCalled cl)
  duration <- parseDuration (clDuration cl)
  return $ call (clCostumerId cl) called duration
