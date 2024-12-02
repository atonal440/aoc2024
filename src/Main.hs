module Main where

import Days ( dispatch )
import Data.Char ( toLower )
import System.Environment ( getArgs )

main :: IO ()
main = do
  dayArg : partArg : inputFilePath : _ <- getArgs
  rawInput <- readFile inputFilePath
  let
    day = read @Int dayArg
    part = toLower <$> partArg
    result = dispatch day part rawInput
  putStrLn result
