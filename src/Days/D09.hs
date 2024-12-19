{-# OPTIONS_GHC -Wno-x-partial #-}
module Days.D09 where

import Lib ( Dispatch, dispatchWith )
import Data.List qualified as List
import Data.Maybe ( catMaybes, listToMaybe, fromJust )
import Data.Vector ( Vector )
import Data.Vector qualified as Vec
import Control.Arrow ( (&&&) )
import Data.Map ( Map )
import Data.Map qualified as Map

dispatch :: Dispatch
dispatch = dispatchWith part1 part2

part1 :: String -> Int
part1 = checksum . compact . parseInput

part2 :: String -> Int
part2 = checksum . defrag . parseInput

type Sector = Maybe Int

checksum :: [Sector] -> Int
checksum = sum . catMaybes . zipWith (liftA2 (*)) (Just <$> [0..])

compact :: [Sector] -> [Sector]
compact sectors
  = reverse . snd $ foldl' go ((filesFromEnd, filesCount), []) sectors
  where
  filesOnly = catMaybes sectors
  filesFromEnd = reverse filesOnly
  filesCount = length filesOnly
  go acc@((_, 0),_) _ = acc
  go ((compactable, remaining), compacted) Nothing =
    ( ( drop 1 compactable , pred remaining )
    , Just (head compactable) : compacted
    )
  go ((compactable, remaining), compacted) file =
    ( (compactable, pred remaining)
    , file : compacted
    )

defrag :: [Sector] -> [Sector]
defrag sectors = Vec.toList $ defrag' segments sectorVector
  where
  sectorVector = Vec.fromList sectors
  segments = Map.fromList . fmap mapData . List.group $ catMaybes sectors
  mapData segment = let fileIndex = List.head segment in
    ( fileIndex
    , ( fromJust $ Vec.elemIndex (Just fileIndex) sectorVector
      , length segment
    ) )

defrag' :: Map Int (Int,Int) -> Vector Sector -> Vector Sector
defrag' segments sectors
  | null segments = sectors
  | otherwise = defrag' segments' sectors'
  where
  ((fileId, (fileIx, fileSz)), segments') = Map.deleteFindMax segments
  earliestValidTarget
    = listToMaybe . fmap fst
    . filter ( (>= fileSz) . snd )
    . filter ( (< fileIx) . fst )
    . fmap (List.head &&& List.length)
    . Vec.foldr groupSequential []
    $ Vec.elemIndices Nothing sectors
  sectors' = case earliestValidTarget of
    Nothing -> sectors
    Just target -> sectors Vec.//
      ( mkUpd target fileSz (Just fileId) <> mkUpd fileIx fileSz Nothing )
  mkUpd ix sz val = zip [ix..ix+sz-1] $ repeat val
  groupSequential :: Int -> [[Int]] -> [[Int]]
  groupSequential x [] = [[x]]
  groupSequential x acc@(ys:zs)
    | x == pred (head ys) = (x:ys):zs
    | otherwise           = [x]:acc

--
parseInput :: String -> [Sector]
parseInput = snd . foldl' interleave ((0, False), []) . fmap (read . pure)
  where
  interleave ((fileId, isFree), sectors) size =
    ( ( if isFree then succ fileId else fileId , not isFree )
    , sectors <> List.replicate size (if isFree then Nothing else Just fileId)
    )
