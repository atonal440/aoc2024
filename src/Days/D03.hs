 {-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Days.D03 where

import Lib ( Dispatch, dispatchWith )
import Text.Regex.PCRE
import Debug.Trace (traceShowId)

dispatch :: Dispatch
dispatch = dispatchWith id part1 part2

part1, part2 :: String -> Int
part1 = sum . fmap mulString . parseInput
part2 = snd . foldl' filterMuls (True,0) . traceShowId . parseMoreInput 

--

mulRegex :: String
mulRegex = "mul\\([0-9]{1,3}\\,[0-9]{1,3}\\)"
doRegex :: String
doRegex = "do\\(\\)"
dontRegex :: String
dontRegex = "don't\\(\\)"
combinedRegex :: String
combinedRegex = "(?:" ++ doRegex ++ "|" ++ dontRegex ++ "|" ++ mulRegex ++ ")"

mulString :: String -> Int
mulString str = product . map read . getAllTextMatches $ (str =~ "[0-9]{1,3}") 

filterMuls :: (Bool, Int) -> String -> (Bool, Int)
filterMuls (True, x) "don't()" = (False, x)
filterMuls (_, x) "do()"  = (True, x)
filterMuls (True, x) s = (True, x + mulString s)
filterMuls (False, x) _ = (False, x)



--

parseInput :: String -> [String]
parseInput str = getAllTextMatches (str =~ mulRegex)

parseMoreInput:: String -> [String]
parseMoreInput str = getAllTextMatches (str =~ combinedRegex)