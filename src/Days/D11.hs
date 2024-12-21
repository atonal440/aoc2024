{-# OPTIONS_GHC -Wno-x-partial #-}
module Days.D11 where

import Lib ( Dispatch, dispatchWith, countDigits )
import Data.Bool ( bool )

dispatch :: Dispatch
dispatch = dispatchWith part1 part2

part1 :: String -> Int
part1 = sum . iterateRules 25 . fmap read . words

part2 :: String -> Int
part2 = undefined

iterateRules :: Int -> [Int] -> [Int]
iterateRules n = head . drop n . iterate (concatMap rules)

rules :: Int -> [Int]
rules 0 = [1]
rules x = bool boost split $ digits `mod` 2 == 0
  where
  digits = countDigits x
  boost = [x * 2024]
  split = let
    shift = 10 ^ (digits `div` 2)
    upper = x `div` shift
    lower = x - upper * shift
    in [upper, lower]
