module Main where
import System.Environment ( getArgs )
import Data.List qualified as List


main :: IO ()
main = do
  filePath:_ <- getArgs
  reactorData <- parseInput <$> readFile filePath
  let
    permList = fmap getPerms reactorData
    tuples = fmap (fmap entupleList) permList
    deltas = fmap (fmap getDeltas) tuples
    safeList = fmap (any checkSafety) deltas
    trues = filter id safeList
  print $ length trues

checkSafety :: [Int] -> Bool
checkSafety l 
  | any ( > 3) (map abs l) = False
  | any (<= 0) (map abs l) = False
  | otherwise             = checkConsistency l


checkConsistency :: [Int] -> Bool
checkConsistency l = abs (sum signList) == length l
  where
    signList = map signum l

parseInput :: String -> [[Int]]
parseInput = fmap parseNumbers . lines

parseNumbers :: String -> [Int]
parseNumbers  = fmap read . words

getDeltas :: [(Int,Int)] -> [Int]
getDeltas = fmap delta 
  where
    delta x = snd x - fst x

entupleList :: [Int] -> [(Int,Int)]
entupleList [] = []
entupleList [_] = []
entupleList (a:b) = (a, head b) : entupleList b

getPerms :: [Int] -> [[Int]]
getPerms l =
  l : zipWith (<>) (List.inits l) (drop 1 $ List.tails l)
