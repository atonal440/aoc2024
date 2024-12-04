module Days.D04 where

import Lib ( dispatchWith, Dispatch )
dispatch :: Dispatch
dispatch = dispatchWith words part1 part2
--
part1, part2 :: [String] -> [String]
part1 = eightWaySearch (0,5) 3  
part2 = undefined

getFromLists :: (Int,Int) -> [String] -> [Char]
getFromLists (x,y) s
    | (x < 0) || (y < 0) = ""
    | (x > (length s - 1)) = ""
    | (y > length (s !! 0) - 1) = ""
    | otherwise = s !! x !! y : []

eightWaySearch :: (Int,Int) -> Int -> [String] -> [String]
eightWaySearch (x,y) 0 s = replicate 9 (getFromLists (x,y) s)
eightWaySearch (x,y) l s = zipWith (<>) (eightWaySearch(x,y) (l-1) s) (fmap (flip getFromLists s) coords) 
    where 
        coords = [(x + l * a, y + l * b) | a <- [-1..1], b <- [-1..1]]


-- 

-- parseInput :: String -> [String]
-- parseInput l = reverseLists l <> diagonalLists l <> verticalLists l <> words l

-- reverseLists :: String -> [String]
-- reverseLists l = fmap reverse verticalLists l <> diagonalLists l <> lines l

-- verticalLists :: String -> [String]
-- verticalLists = transpose . words 

-- diagonalLists :: String -> [String]
-- diagonalLists = undefined 