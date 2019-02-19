module Pure.ControlFlow where

import Pure.Domain
import Pure.Rules

pickAWinner :: GameState -> String
pickAWinner gs =
  let dealer = dealerPlayer gs
      dScore = show (score dealer)
      sam = properPlayer gs
      sScore = show (score sam)
  in case (compare dealer sam) of GT -> "The dealer win the game: " ++ dScore ++ " over " ++ sScore
                                  EQ -> "Tie game at " ++ dScore
                                  LT -> (name sam) ++ " wins the game: " ++ sScore ++ " over " ++ dScore

blackjackMessage :: Bool -> Player -> String
blackjackMessage b p =
  let n = name p
      h = hand p
      s = score p
  in if b
    then  n ++ " BLACKJACK!!" ++
          "Hand of " ++ n ++ ": " ++ show h
    else  "Hand of " ++ n ++ ": " ++ show h

lostMessage :: Bool -> Player -> String
lostMessage b p =
  let n = name p
      h = hand p
      s = score p
  in if b
  then  n ++ " HAS LOST!!!(" ++ show s ++ ")\n" ++
        "Hand of " ++ n ++ ": " ++ show h
  else "Hand of " ++ n ++ ": " ++ show h
