{-# OPTIONS_GHC -Wno-type-defaults #-}
module Days.D07 where

import Lib ( Dispatch, dispatchWith )
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
ops2 = catDigits : ops1

run :: OpSet -> [TestSet] -> Int
run ops input = sum $ do
  testSet <- input
  check <- List.nub $ permute testSet ops
  guard $ check == testSet.value
  pure testSet.value

permute :: TestSet -> OpSet -> [Int]
permute TestSet{..} operators = go operands
  where
  go (x:y:zs) = do
    w <- [op x y | op <- operators]
    guard $ w <= value  -- hideous special case but holy shit catDigits is slow
    go $ w : zs
  go xs = xs

-- catDigits :: Int -> Int -> Int
-- catDigits x y = (magnitude y * x) + y
--   where magnitude = (10 ^) . (1 +) . floor . logBase 10 . fromIntegral

-- this is 25% faster?!
catDigits :: Int -> Int -> Int
catDigits x y = read $ show x <> show y

parseInput :: String -> [TestSet]
parseInput = fmap parseLine . lines
  where
  parseLine = uncurry TestSet . bimap read parseOperands . break (==':')
  parseOperands = fmap read . words . drop 1
