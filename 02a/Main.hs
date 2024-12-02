module Main where

import Control.Arrow ((&&&))
import Data.Bool (bool)
import System.Environment ( getArgs )

main :: IO ()
main = do
  filePath:_ <- getArgs
  input <- parseInput <$> readFile filePath
  let result = length . filter safeSystem $ input
  print result

parseInput :: String -> [[Int]]
parseInput = fmap (fmap read . words) . lines

safeSystem :: [Int] -> Bool
safeSystem = fst . foldl safeStep (True, (0, 0))

type Signum = Int
unsafe :: (Bool, (Signum, Int))
unsafe = (False, (0, 0))
safe :: (Signum, Int) -> (Bool, (Signum, Int))
safe = (True,)
safeStep :: (Bool, (Signum, Int)) -> Int -> (Bool, (Signum, Int))
safeStep (False, _     ) _ = unsafe
safeStep (_    , (0, 0)) x = safe (0, x)
safeStep (_    , (s, w)) x = bool unsafe (safe (s', x)) isSafe
  where
  (s', d) = (signum &&& abs) $ x - w
  isSafe = (s == 0 || s == s') && d >= 1 && d <= 3
