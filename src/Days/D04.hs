module Days.D04 where

import Lib ( dispatchWith, Dispatch )
dispatch :: Dispatch
dispatch = dispatchWith words part1 part2
--
part1,part2 :: [String] -> Int
part1 s = length (filter xmasFilter (permsList s))
part2 s =  length (filter crossMasFilter (permsList' s))

getFromLists :: (Int,Int) -> [String] -> [Char]
getFromLists (x,y) s
    | (x < 0) || (y < 0) = ""
    | (x > (length s - 1)) = ""
    | (y > length (s !! 0) - 1) = ""
    | otherwise = s !! x !! y : []

eightWaySearch :: Int -> [String] -> (Int,Int)  -> [String]
eightWaySearch 0 s (x,y) = replicate 9 (getFromLists (x,y) s)
eightWaySearch l s (x,y) = zipWith (<>) (eightWaySearch (l-1) s (x,y)) (fmap (flip getFromLists s) coords) 
    where 
        coords = [(x + l * a, y + l * b) | a <- [-1..1], b <- [-1..1]]

xmasFilter :: String -> Bool
xmasFilter s = "XMAS" == s

coordList ::[String] -> [(Int,Int)]
coordList s = [(x,y)| x <- [0..ubx], y <- [0..uby]] 
    where
        ubx = length s - 1
        uby = length (s !! 0) - 1

permsList :: [String] -> [String]
permsList s = concat (fmap (eightWaySearch 3 s) (coordList s))

fourWaySearch :: (Int,Int)-> [String] -> (Int,Int)  -> [String]
fourWaySearch (l,i) s (x,y) 
    | l == -i = fmap (flip getFromLists s) coords
    | otherwise = zipWith (<>) (fourWaySearch (l, i-1) s (x,y)) (fmap (flip getFromLists s) coords) 
        where 
            coords = [(x + i * a, y + i * b) | a <- [-1,1], b <- [-1,1]]

permsList' :: [String] -> [[String]]
permsList' s = (fourWaySearch (1,1) s) <$> (coordList s)

crossMasFilter :: [String] -> Bool
crossMasFilter xs = 2 == (length (filter (== "MAS") xs))