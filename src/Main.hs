module Main where

import Days ( dispatch )
import Data.Bifunctor ( bimap )
import Data.Char ( toLower, isAlpha )
import Data.List qualified as List
import System.Environment ( getArgs )

main :: IO ()
main = do
  dispatchArg : inputFilePath : _ <- getArgs
  rawInput <- readFile inputFilePath
  let
    (day, part) = parseDispatchArg dispatchArg
    result = dispatch day part rawInput
  putStrLn result

parseDispatchArg :: String -> (Int, String)
parseDispatchArg = bimap read (fmap toLower) . List.break isAlpha
