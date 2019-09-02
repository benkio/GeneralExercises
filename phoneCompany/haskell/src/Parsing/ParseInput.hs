{-# LANGUAGE RecordWildCards #-}

module Parsing.ParseInput where

import Pure.Domain
import Pure.Transformation
import Data.List.Split
import Data.Maybe
import System.IO

parseCallsLog :: String -> IO [CallLog]
parseCallsLog inputFile = do
  callLogLines <- lines <$> readFile inputFile
  let callLogs = filter ((==) 3 . length) $ fmap words callLogLines
  return $ fmap listToRecord  callLogs
  where
    listToRecord l = CallLog {..}
      where [clCostumerId, clCalled, clDuration] = l

parseCalls ::  String -> IO [Call]
parseCalls inputFile = do
  callLogs <- parseCallsLog inputFile
  let maybeCalls = fmap validateInput callLogs
  return $ catMaybes maybeCalls
