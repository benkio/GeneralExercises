{-# LANGUAGE RecordWildCards #-}

module Parsing.ParseInput where

-- TODO: IMPLEMENT
import Pure.Domain
import Data.List.Split
import System.IO

parseCallsLog :: String -> IO [CallLog]
parseCallsLog inputFile = do
  callLogLines <- lines <$> readFile inputFile
  let callLogs = filter ((==) 3 . length) $ fmap words callLogLines
  return $ fmap listToRecord  callLogs
  where
    listToRecord l = CallLog {..}
      where [clCostumerId, clCalled, clDuration] = l
