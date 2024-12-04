module Days.D02 where

import Lib ( Dispatch, dispatchWith )
import Control.Arrow ( (&&&) )
import Data.Bool ( bool )

dispatch :: Dispatch
dispatch = dispatchWith (part1 . parseInput) (part2 . parseInput)
--

part1, part2 :: [[Int]] -> Int
part1 = length . filter safeSystem
part2 = length . filter problemDampener

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

problemDampener :: [Int] -> Bool
problemDampener levels = (not . null) . filter id $ safeSystem <$> alternates
  where
  count = length levels
  indices = [1 .. count]
  ixLevels = replicate count $ zip indices levels
  candidateSet = zip indices ixLevels
  prune (ix, candidate) = snd <$> filter ((/= ix) . fst) candidate
  alternates = levels : fmap prune candidateSet

--
parseInput :: String -> [[Int]]
parseInput = fmap (fmap read . words) . lines
