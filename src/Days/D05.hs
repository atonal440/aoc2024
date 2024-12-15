{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}
module Days.D05 where

import Lib ( Dispatch, dispatchWith, both )
import Data.List ( sortBy )
import Data.List.Split( splitOn )
import Data.Map.Strict qualified as Map
import Data.Tuple ( swap )
import Data.Maybe( fromMaybe )

dispatch :: Dispatch
dispatch = dispatchWith lines part1 part2

part1 :: [String] -> Int
part1 xs = sum . fmap middle . filter (isOrdered $ makeRules . parseInstructions $ xs) $ (parseLists xs)
part2 :: [String] -> Int
part2 xs = (sum . fmap middle . fmap (sortBy $ rulesLookup $ makeRules . parseInstructions $ xs) $ (parseLists xs))- part1 xs

--
 
type RulesMap = Map.Map (Int,Int) Ordering

--

middle :: [Int] -> Int
middle xs = head . drop ((length xs - 1 ) `div` 2) $ xs

isOrdered :: RulesMap -> [Int] -> Bool
isOrdered rules list = (sortBy (rulesLookup rules) list) == list

rulesLookup :: RulesMap -> Int -> Int -> Ordering
rulesLookup rules x y = fromMaybe EQ (Map.lookup (x,y) rules)

parseInstructions :: [String] -> [(Int,Int)]
parseInstructions = map tupleDigits . filter (elem '|')

tupleDigits :: String -> (Int,Int)
tupleDigits xs = both read (fst t, tail (snd t)) 
    where
        t = break ( == '|') xs

makeRules :: [(Int,Int)] -> RulesMap
makeRules xs = Map.fromList (ltList ++ gtList)
    where
        ltList = zip xs (repeat LT)
        gtList = zip (map swap xs) (repeat GT)

parseLists :: [String] -> [[Int]]
parseLists = fmap readAll . map (splitOn ",") . filter ( elem ',')
    where
        readAll :: [String] -> [Int]
        readAll = fmap read

