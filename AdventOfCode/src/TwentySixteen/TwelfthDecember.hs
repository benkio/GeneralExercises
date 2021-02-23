module TwentySixteen.TwelfthDecember where

data Instruction =
  CPY (Either Int String) String
  | INC String
  | DEC String
  | JNZ String Int

input :: IO String
input = readFile "input/2016/12December.txt"

solution1 :: String -> Int
solution1 = undefined

twelfthDecemberSolution1 :: IO Int
twelfthDecemberSolution1 = undefined

solution2 :: String -> Int
solution2 = undefined

twelfthDecemberSolution2 :: IO Int
twelfthDecemberSolution2 = undefined
