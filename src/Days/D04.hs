{-# LANGUAGE OverloadedStrings #-}
module Days.D04 where

import Lib ( Dispatch, dispatchWith )
import Control.Monad ( void )
import Data.Text qualified as Text
import Data.Text ( Text )
import Data.Map qualified as Map

dispatch :: Dispatch
dispatch = dispatchWith parseInput part1 undefined
--

part1 :: [Text] -> Int
part1 input = length $ do
  word <- searchWords
  line <- input
  void $ Text.breakOnAll word line
  where
  searchWords = ["XMAS"]

--
parseInput :: String -> [Text]
parseInput input = Text.pack <$> forward <> backward
  where
  backward = reverse <$> forward
  forward = rows <> cols <> diag1 <> diag2 <> diag3 <> diag4
  rows = lines input
  cols = columns rows
  diag1 = columns . drops $ rows
  diag2 = columns . drops $ reverse rows
  diag3 = drop 1 . columns . drops . fmap reverse $ rows
  diag4 = drop 1 . columns . drops . fmap reverse $ reverse rows

columns :: [String] -> [String]
columns rows = fmap snd . Map.toList $ Map.unionsWith (<>) ixMaps
  where
  ixMaps = fmap (Map.fromList) ixRows
  ixRows = zip [(1::Int)..] . fmap pure <$> rows

drops :: [String] -> [String]
drops = snd . foldr shrinkinator (0, [])
  where
  shrinkinator xx (n, aa) = (succ n, drop n xx : aa)
