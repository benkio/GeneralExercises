module TwentySixteen.TwentyfourthDecember where

-- the idea is to have a:
--  - Map Coordinate Status indicate what is in the map
--  - The initial position (Coordinate, Int) that is the position of the robot.
--  - A function taking the list of Current Coordinates & the visited
--    coordinates. It returns the available next coordinates that are not
--    into the visited and the updated visited, adding the starting
--    points to the visited.
--  - Run the previous function with the initial position until a number
--    is found. At each call increase a number that is the number of
--    steps.
--  - use the previous function again to find the first element to visit,
--    collect it, update the position found with an empty space. Redo
--    until all elements are reached.
--  - sum up the steps to reach each elements. profit

input :: IO String
input = readFile "input/2016/24December.txt"

solution1 :: String -> Int
solution1 = undefined

twentyfourthDecemberSolution1 :: IO Int
twentyfourthDecemberSolution1 = undefined

solution2 :: String -> Int
solution2 = undefined

twentyfourthDecemberSolution2 :: IO Int
twentyfourthDecemberSolution2 = undefined
