module Days.D07 where

import Lib ( Dispatch, dispatchWith )
import Data.Bifunctor ( bimap )
import Control.Monad ( guard )
import Debug.Trace (traceShowId)
import Data.List qualified as List

dispatch :: Dispatch
dispatch = dispatchWith part1 part2

part1 :: String -> Int
part1 = sum . traceShowId . run1 . parseInput

part2 :: String -> Int
part2 = undefined

data TestSet = TestSet
  { value    :: Int
  , operands :: [Int]
  } deriving Show

run1 :: [TestSet] -> [Int]
run1 input = do
  testSet <- input
  check <- List.nub $ permute testSet.operands
  guard $ check == testSet.value
  pure testSet.value
  where
  permute :: [Int] -> [Int]
  permute (x:y:zs) = do
    w <- [op x y | op <- [(+), (*)]]
    permute $ w : zs
  permute xs = xs

parseInput :: String -> [TestSet]
parseInput = fmap parseLine . lines
  where
  parseLine = uncurry TestSet . bimap read parseOperands . break (==':')
  parseOperands = fmap read . words . drop 1
