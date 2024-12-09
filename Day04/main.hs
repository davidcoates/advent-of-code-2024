import Data.Array
import Prelude hiding (words)

type Grid = Array (Int, Int) Char

parseInput :: IO Grid
parseInput = do
  contents <- readFile "input.txt"
  let n = length (lines contents)
      bounds = ((1, 1), (n, n))
  return $ listArray bounds (concat (lines contents))

-- helper for all the words in direction (dx, dy) of length n
words :: Grid -> (Int, Int) -> Int -> [String]
words grid (dx, dy) n = [
  map (grid !) indices
    | (x, y) <- range (bounds grid)
    , let indices = [ (x + step * dx, y + step * dy) | step <- [0..n-1] ]
    , all (inRange (bounds grid)) indices
  ]

-- helper for all nxm windows
windows :: Grid -> (Int, Int) -> [Grid]
windows grid (n, m) = [
  listArray ((1, 1), (n, m)) (map (grid !) indices)
    | (x, y) <- range (bounds grid)
    , let indices = [ (x + dx, y + dy) | dx <- [0..n-1], dy <- [0..m-1] ]
    , all (inRange (bounds grid)) indices
  ]

part1 :: IO ()
part1 = do
  grid <- parseInput
  let directions = [ (dx, dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx /= 0 || dy /= 0 ]
  let answer = length [ word | direction <- directions, word <- words grid direction 4, word == "XMAS" ]
  print answer

part2 :: IO ()
part2 = do
  grid <- parseInput
  let answer = length [
        window
          | window <- windows grid (3, 3)
          , "MAS" `elem` (words window (1,  1) 3 ++ words window (-1, -1) 3)
          , "MAS" `elem` (words window (1, -1) 3 ++ words window (-1,  1) 3)
        ]
  print answer

main :: IO ()
main = do
  part1
  part2
