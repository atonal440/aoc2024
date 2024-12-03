{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Days.D03 where

import Lib
import Text.Regex.TDFA ( (=~), AllTextMatches(getAllTextMatches) )
import Data.List qualified as List
import Data.Char (isNumber)

dispatch :: Dispatch
dispatch = dispatchWith parseInput part1 undefined
--

part1 :: String -> Int
part1 = sum . fmap exec . parseInput

data Expr
  = Val Int
  | Mul Expr Expr

exec :: Expr -> Int
exec (Mul x y) = exec x * exec y
exec (Val x) = x

---
parseInput :: String -> [Expr]
parseInput = fmap parseMul . extractMuls
  where
  matchMul :: String
  matchMul = "mul\\([0-9]{1,3},[0-9]{1,3}\\)"
  extractMuls input = getAllTextMatches (input =~ matchMul)

parseMul :: String -> Expr
parseMul mulStr = Mul (Val x) (Val y)
  where
  [x, y] = fmap read . filter isNumGroup $ List.groupBy groupNumbers mulStr
  isNumGroup = any isNumber . List.take 1
  groupNumbers c1 c2 = isNumber c1 == isNumber c2
