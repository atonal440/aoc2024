module Main where

import Lib ( both )
import System.Environment ( getArgs )
import Data.List qualified as List

main :: IO ()
main = do
  filePath:_ <- getArgs
  input <- parseInput <$> readFile filePath
  let
    sorted = both List.sort input
    diffs = uncurry (zipWith difference) sorted
  print $ sum diffs

difference :: Int -> Int -> Int
difference a b = abs (a - b)

parseInput :: String -> ([Int], [Int])
parseInput = unzip . fmap parsePair . lines

parsePair :: String -> (Int, Int)
parsePair = entuple . fmap read . words
  where
  entuple [a, b] = (a, b)
  entuple _ = error "tried to entuple a non-pair list"
