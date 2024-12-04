{-# OPTIONS_GHC -fno-warn-x-partial #-}
module Days.D01 where

import Lib ( Dispatch, dispatchWith, both )
import Control.Arrow ( (&&&) )
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Map (Map)

dispatch :: Dispatch
dispatch = dispatchWith (part1 . parseInput) (part2 . parseInput)
--

part1 :: ([Int], [Int]) -> Int
part1 input =
  let
    sorted = both List.sort input
    diffs = uncurry (zipWith difference) sorted
  in sum diffs

part2 :: ([Int], [Int]) -> Int
part2 (col1, col2)
  = sum $ applyMultipliers col1 $ getMultipliers col2

difference :: Int -> Int -> Int
difference a b = abs (a - b)

applyMultipliers :: [Int] -> Map Int Int -> [Int]
applyMultipliers vals mults = go <$> vals
  where
  go x = x * Map.findWithDefault 0 x mults

getMultipliers :: [Int] -> Map Int Int
getMultipliers
  = Map.fromList . fmap (head &&& length) . List.group . List.sort

--
parseInput :: String -> ([Int], [Int])
parseInput = unzip . fmap parsePair . lines

parsePair :: String -> (Int, Int)
parsePair = entuple . fmap read . words
  where
  entuple [a, b] = (a, b)
  entuple _ = error "tried to entuple a non-pair list"
