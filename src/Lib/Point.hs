module Lib.Point where

import Linear ( V2(..) )

type Point = V2 Int

{-# COMPLETE Point #-}
pattern Point :: Int -> Int -> V2 Int
pattern Point x y = V2 x y

(.<=) :: Point -> Point -> Bool
(Point x y) .<= (Point x' y') = x <= x' && y <= y'

bounded :: Point -> Point -> Bool
bounded bounds point = point .<= bounds && Point 0 0 .<= point
