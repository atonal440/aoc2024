{-# OPTIONS_GHC -Wno-x-partial #-}
module Days.D09 where

import Lib ( Dispatch, dispatchWith )
import Data.List qualified as List
import Data.Maybe (catMaybes)

dispatch :: Dispatch
dispatch = dispatchWith part1 part2

part1 :: String -> Int
part1 = checksum . compact . parseInput

part2 :: String -> Int
part2 = undefined

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

parseInput :: String -> [Sector]
parseInput = snd . foldl' interleave ((0, False), []) . fmap (read . pure)
  where
  interleave ((fileId, isFree), sectors) size =
    ( ( if isFree then succ fileId else fileId , not isFree )
    , sectors <> List.replicate size (if isFree then Nothing else Just fileId)
    )
