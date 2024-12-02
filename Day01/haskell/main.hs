import Data.List (sort)

readInputAsColumns :: IO ([Int], [Int])
readInputAsColumns = do
  contents <- readFile "../input.txt"
  let parseLine :: String -> (Int, Int)
      parseLine = (\[a, b] -> (a, b)) . map read . words
  let parsedLines = map parseLine (lines contents) :: [(Int, Int)]
  return $ unzip parsedLines

part1 :: IO ()
part1 = do
  (column0, column1) <- readInputAsColumns
  let totalDistance = sum $ zipWith (\x y -> abs (x - y)) (sort column0) (sort column1)
  print totalDistance

part2 :: IO ()
part2 = do
  (column0, column1) <- readInputAsColumns
  let count :: Eq a => a -> [a] -> Int
      count _     [] = 0
      count x (y:ys) = (if x == y then 1 else 0) + count x ys
  let totalSimilarity = sum $ map (\x -> x * count x column1) column0
  print totalSimilarity  

main :: IO ()
main = do
  part1
  part2
