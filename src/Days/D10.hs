{-# OPTIONS_GHC -Wno-x-partial #-}
module Days.D10 where

import Lib ( Dispatch, dispatchWith, unvec )
import Lib.Field (Field, Point, pattern Point)
import Lib.Field qualified as Field
import Control.Arrow ( (&&&) )
import Control.Monad ( guard )
import Data.Maybe ( catMaybes )
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Vector qualified as Vec

dispatch :: Dispatch
dispatch = dispatchWith part1 part2

part1 :: String -> Int
part1 = length . uniqueEndpointTrails . allTrails . parseInput

part2 :: String -> Int
part2 = length . allTrails . parseInput

uniqueEndpointTrails :: [[Point]] -> [(Point, Point)]
uniqueEndpointTrails = List.nub . fmap (head &&& last)

allTrails :: Field Int -> [[Point]]
allTrails field = trailsFrom 9 . fmap pure $ elevationPoints 9
  where
  elevationPoints :: Int -> [Point]
  elevationPoints = catMaybes . sequence . flip Map.lookup elevationMap
  elevationMap = Map.fromList . unvec . Vec.generate 10
    $ \n -> (n, unvec $ Field.elemIndices n field)
  adjacent = [Point 0 1, Point 0 (-1), Point 1 0, Point (-1) 0]
  trailsFrom :: Int -> [[Point]] -> [[Point]]
  trailsFrom 0    = id
  trailsFrom step = \trails -> do
    trail <- trails
    loc <- take 1 trail
    let step' = pred step
    next <- elevationPoints step'
    guard $ loc - next `elem` adjacent
    trailsFrom step' [next : trail]

parseInput :: String -> Field Int
parseInput = Field.parse (read . pure)
