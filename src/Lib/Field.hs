module Lib.Field where

import Data.Vector ( Vector, (!?) )
import Data.Vector qualified as Vec
import Linear ( V2(..) )

type Field a = Vector (Vector a)
type Point = V2 Int

{-# COMPLETE Point #-}
pattern Point :: Int -> Int -> V2 Int
pattern Point x y = V2 x y

lookup :: Field a -> Point -> Maybe a
lookup field (Point x y) = (!? x) =<< (field !? y)

elemIndices :: (Show a, Eq a) => a -> Field a -> Vector Point
elemIndices x field = do
  (colIx, row) <- Vec.imap (,) field
  rowIx <- Vec.elemIndices x row
  pure $ Point rowIx colIx

parse :: (Char -> a) -> String -> Field a
parse parseChar = vec . fmap (vec . fmap parseChar) . lines
  where vec = Vec.force . Vec.fromList
