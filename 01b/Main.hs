module Main where
import System.Environment ( getArgs )


main :: IO ()
main = do
  filePath:_ <- getArgs
  (lista, listb) <- parseInput <$> readFile filePath
  let
    simList = fmap (flip simNum listb) lista
  print $ sum simList

simNum :: Int -> [Int] -> Int
simNum x = sum . filter(==x)

parseInput :: String -> ([Int], [Int])
parseInput = unzip . fmap parseNumbers . lines

parseNumbers :: String -> (Int, Int)
parseNumbers  = entuple . fmap read . words
  where
    entuple [a, b] = (a, b)
    entuple _ = error "not a pair"