module Main where
import System.Environment ( getArgs )
import qualified Data.List as List
import Lib ( both )


main :: IO ()
main = do
  filePath:_ <- getArgs
  input <- parseInput <$> readFile filePath
  let
    sorted = both List.sort input
    diffs = uncurry (zipWith difference) sorted
  print $ sum diffs

difference :: Int -> Int -> Int
difference a b = abs(a - b)

parseInput :: String -> ([Int], [Int])
parseInput = unzip . fmap parseNumbers . lines

parseNumbers :: String -> (Int, Int)
parseNumbers  = entuple . fmap read . words
  where
    entuple [a, b] = (a, b)
    entuple _ = error "not a pair"