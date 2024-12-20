module Days.D07 where

import Lib ( Dispatch, dispatchWith, countDigits )
import Data.Bifunctor ( bimap )
import Control.Monad ( guard )
import Data.List qualified as List

dispatch :: Dispatch
dispatch = dispatchWith part1 part2

part1 :: String -> Int
part1 = run ops1 . parseInput

part2 :: String -> Int
part2 = run ops2 . parseInput

data TestSet = TestSet
  { value    :: Int
  , operands :: [Int]
  } deriving Show

type OpSet = [(Int -> Int -> Int)]
ops1, ops2 :: OpSet
ops1 = [(+), (*)]
ops2 = ops1 <> [catDigits]

run :: OpSet -> [TestSet] -> Int
run operators input = sum $ do
  testSet <- input
  List.take 1 $ permute operators testSet

permute :: OpSet -> TestSet -> [Int]
permute operators TestSet{..} = go operands
  where
  go (x:y:zs) = do
    w <- [op x y | op <- operators]
    guard $ w <= value
    go $ w : zs
  go [x] | x == value = [x]
  go _ = []

catDigits :: Int -> Int -> Int
catDigits x y = (10 ^ (countDigits y) * x) + y

parseInput :: String -> [TestSet]
parseInput = fmap parseLine . lines
  where
  parseLine = uncurry TestSet . bimap read parseOperands . break (==':')
  parseOperands = fmap read . words . drop 1
