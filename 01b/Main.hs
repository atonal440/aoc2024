module Main where

import Control.Arrow ( Arrow((&&&)) ) 
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import System.Environment ( getArgs )

main :: IO ()
main = do
  filePath:_ <- getArgs
  (col1, col2) <- parseInput <$> readFile filePath
  print $ sum $ applyMultipliers col1 $ getMultipliers col2

applyMultipliers :: [Int] -> Map Int Int -> [Int]
applyMultipliers vals mults = go <$> vals
  where
  go x = x * Map.findWithDefault 0 x mults

getMultipliers :: [Int] -> Map Int Int
getMultipliers = Map.fromList . fmap (head &&& length) . List.group . List.sort

parseInput :: String -> ([Int], [Int])
parseInput = unzip . fmap parsePair . lines

parsePair :: String -> (Int, Int)
parsePair = entuple . fmap read . words
  where
  entuple [a, b] = (a, b)
  entuple _ = error "tried to entuple a non-pair list"

module Main where
import System.Environment ( getArgs )


main :: IO ()
main = do
  filePath:_ <- getArgs
  (lista, listb) <- parseInput <$> readFile filePath
  let
    simList = fmap (flip simNum listb) lista
  print $ sum simList

simNum :: Int -> [Int] -> Int
simNum x = sum . filter(==x)

parseInput :: String -> ([Int], [Int])
parseInput = unzip . fmap parseNumbers . lines

parseNumbers :: String -> (Int, Int)
parseNumbers  = entuple . fmap read . words
  where
    entuple [a, b] = (a, b)
    entuple _ = error "not a pair"