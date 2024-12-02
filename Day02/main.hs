{-# LANGUAGE LambdaCase #-}

readReports :: IO [[Int]]
readReports = do
  contents <- readFile "input.txt"
  return $ map (map read . words) (lines contents)

isReportSafeInc :: [Int] -> Bool
isReportSafeInc = \case
  (x:y:ys) -> (x + 1 <= y && y <= x + 3) && isReportSafeInc (y:ys)
  _ -> True

part1 :: IO ()
part1 = do
  reports <- readReports
  let
    isReportSafe :: [Int] -> Bool
    isReportSafe report = isReportSafeInc report || isReportSafeInc (reverse report)
  let numSafeReports = length . filter isReportSafe $ reports
  print numSafeReports

remove1 :: [a] -> [[a]]
remove1 = \case
  [x] -> [[]]
  (x:xs) -> xs : [ x : xs' | xs' <- remove1 xs ]

part2 :: IO ()
part2 = do
  reports <- readReports
  let
    isReportSafeInc' :: [Int] -> Bool
    isReportSafeInc' report = isReportSafeInc report || any isReportSafeInc (remove1 report)
    isReportSafe :: [Int] -> Bool
    isReportSafe report = isReportSafeInc' report || isReportSafeInc' (reverse report)
  let numSafeReports = length . filter isReportSafe $ reports
  print numSafeReports

main :: IO ()
main = do
  part1
  part2
