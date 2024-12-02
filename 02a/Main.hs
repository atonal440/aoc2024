module Main where
import System.Environment ( getArgs )


main :: IO ()
main = do
  filePath:_ <- getArgs
  reactorData <- parseInput <$> readFile filePath
  let
    tuples = fmap entupleList reactorData
    deltas = fmap getDeltas tuples
    safeList =  filter checkSafety deltas
  print $ length safeList 
  
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
