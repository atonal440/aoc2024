{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Days.D03 where

import Lib ( Dispatch, dispatchWith )
import Data.Text qualified as Text
import Text.Regex.TDFA ( (=~), AllTextMatches(getAllTextMatches) )
import Data.List qualified as List
import Data.Char (isNumber)

dispatch :: Dispatch
dispatch = dispatchWith part1 part2
--

part1, part2 :: String -> Int
part1 = sum . fmap exec . parseInput
part2 = part1 . thereIsNoTry

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

thereIsNoTry :: String -> String
thereIsNoTry input = onlyDos
  where
  Just (beforeFirstDont, dontGroups)
    = List.uncons . Text.splitOn "don't()" $ Text.pack input
  doAfterDont = snd . Text.breakOn "do()" <$> dontGroups
  doGroups = beforeFirstDont : doAfterDont
  onlyDos = Text.unpack $ Text.concat doGroups

parseMul :: String -> Expr
parseMul mulStr = Mul (Val x) (Val y)
  where
  [x, y] = fmap read . filter isNumGroup $ List.groupBy groupNumbers mulStr
  isNumGroup = any isNumber . List.take 1
  groupNumbers c1 c2 = isNumber c1 == isNumber c2
