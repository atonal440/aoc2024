module Lib.Point
( Point, pattern Point, px, py
, bounded, (.<=)
) where

import Linear ( V2(..) )

type Point = V2 Int

{-# COMPLETE Point #-}
pattern Point :: Int -> Int -> V2 Int
pattern Point{px, py} = V2 px py

bounded :: Point -> Point -> Bool
bounded bounds point = point .<= bounds && Point 0 0 .<= point

(.<=) :: Point -> Point -> Bool
(Point x y) .<= (Point x' y') = x <= x' && y <= y'
