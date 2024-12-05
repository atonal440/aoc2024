module Days.D05 where

import Lib ( Dispatch, dispatchWith, both )
import Data.Bifunctor ( Bifunctor(bimap), second )
import Data.Maybe (catMaybes)
import Data.List qualified as List

dispatch :: Dispatch
dispatch = dispatchWith part1 part2

--
type Rule = (Int, Int) -> Maybe Ordering
type Update = [Int]

part1 :: String -> Int
part1 input = sum $ middles validUpdates
  where
  (rules, updates) = parse1 input
  validUpdates = filter (isValidUpdate rules) updates

middles :: [[a]] -> [a]
middles = concatMap middle
  where middle xs = take 1 $ drop (length xs `div` 2) xs

applyRules :: [Rule] -> (Int, Int) -> Ordering
applyRules rules pair = case catMaybes $ fmap ($ pair) rules of
  []    -> EQ
  ord:_ -> ord

isValidUpdate :: [Rule] -> Update -> Bool
isValidUpdate rules upd = upd == (List.sortBy (curry $ applyRules rules) upd)

parse1 :: String -> ([Rule], [Update])
parse1 = bimap (parseRules) (parseUpdates . drop 1) . break (=="") . lines

parseRules :: [String] -> [Rule]
parseRules = fmap parseRule where
  parseRule = package . both (read @Int) . second (drop 1) . break (=='|')
  package :: (Int, Int) -> (Int, Int) -> Maybe Ordering
  package (x,y) (x',y')
    | x' == x && y' == y = Just LT
    | x' == y && y' == x = Just GT
  package _ _ = Nothing

parseUpdates :: [String] -> [Update]
parseUpdates = fmap $ read . ('[':) . (<>"]")

part2 :: String -> Int
part2 = undefined
