import Data.Set (Set)
import qualified Data.Set as Set


-- list helpers

split :: (a -> Bool) -> [a] -> [[a]]
split p xs = case break p xs of
  (xs, [])   -> [xs]
  (xs, _:ys) -> xs : split p ys

middle :: [a] -> a
middle [x, y, z] = y
middle ls = middle (tail . init $ ls)

swap :: Eq a => (a, a) -> [a] -> [a]
swap _ [] = []
swap (a, b) (x:xs) = x' : swap (a, b) xs where
  x'
    | x == a    = b
    | x == b    = a
    | otherwise = x


-- parsing

type Rule = (Int, Int)

type Update = [Int]

parseInput :: IO (Set Rule, [Update])
parseInput = do
  contents <- readFile "input.txt"
  let [rules, updates] = split null (lines contents)
  let parseRule :: String -> Rule
      parseRule = (\[a, b] -> (read a, read b)) . split (== '|')
  let parseUpdate :: String -> Update
      parseUpdate = map read . split (== ',')
  return (Set.fromList (map parseRule rules), map parseUpdate updates)


-- helpers

impliedRules :: Update -> [Rule]
impliedRules [] = []
impliedRules (p:ps) = [ (p, p') | p' <- ps ] ++ impliedRules ps

flipRule :: Rule -> Rule
flipRule (a, b) = (b, a)

violatedRules :: Set Rule -> Update -> [Rule]
violatedRules rules update = [ violatedRule | rule <- impliedRules update, let violatedRule = flipRule rule, violatedRule `Set.member` rules ]

isCorrectlyOrdered :: Set Rule -> Update -> Bool
isCorrectlyOrdered rules update = null (violatedRules rules update)

part1 :: IO ()
part1 = do
  (rules, updates) <- parseInput
  let answer = sum [ middle update | update <- updates, isCorrectlyOrdered rules update ]
  print answer

part2 :: IO ()
part2 = do
  (rules, updates) <- parseInput
  let incorrectlyOrderedUpdates = [ update | update <- updates, not (isCorrectlyOrdered rules update) ]
  let fixOrder :: Update -> Update
      fixOrder update = case violatedRules rules update of
        []       -> update
        (rule:_) -> fixOrder (swap rule update)
  let answer = sum [ middle (fixOrder update) | update <- incorrectlyOrderedUpdates ]
  print answer

main :: IO ()
main = do
  part1
  part2
