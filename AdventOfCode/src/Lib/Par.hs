module Lib.Par (findPar) where

import Control.Parallel.Strategies
import Data.List.Split (chunksOf)
import GHC.Data.Maybe (firstJusts)
import Lib.List (find')

numThreads :: Int
numThreads = 8

-- Do not use infinite lists
findPar :: (a -> Bool) -> [a] -> Maybe a
findPar f [] = Nothing
findPar f xs =
    let
        chunks = chunksOf (length xs `div` numThreads) xs
     in
        case chunks of
            [] -> Nothing
            (firstChunk : restChunks) ->
                case find' f firstChunk of
                    Just x -> Just x
                    Nothing ->
                        let results = fmap (find' f) restChunks `using` parBuffer (numThreads - 1) rseq
                         in firstJusts results
