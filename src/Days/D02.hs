module Days.D02 where

import Lib ( Dispatch, dispatchWith )
import Data.List qualified as List

dispatch :: Dispatch
dispatch = dispatchWith parseInput part1 part2
--

part1, part2 :: [[Int]] -> Int
part1 = length . filter checkSafety . differenceList
part2 = length . filter (any checkSafety) . fmap differenceList . fmap getPerms

checkSafety :: [Int] -> Bool
checkSafety l 
  | any ( > 3) (map abs l) = False
  | any (<= 0) (map abs l) = False
  | otherwise              = checkConsistency l

checkConsistency :: [Int] -> Bool
checkConsistency l = abs (sum signList) == length l
  where
    signList = map signum l

getDeltas :: [(Int,Int)] -> [Int]
getDeltas = fmap delta 
  where
    delta x = snd x - fst x

entupleList :: [Int] -> [(Int,Int)]
entupleList [] = []
entupleList [_] = []
entupleList (a:b) = (a, head b) : entupleList b

differenceList :: [[Int]] -> [[Int]]
differenceList = fmap getDeltas . fmap entupleList

getPerms :: [Int] -> [[Int]]
getPerms l =
  l : zipWith (<>) (List.inits l) (drop 1 $ List.tails l)


--
parseInput :: String -> [[Int]]
parseInput = fmap parseNumbers . lines

parseNumbers :: String -> [Int]
parseNumbers  = fmap read . words

