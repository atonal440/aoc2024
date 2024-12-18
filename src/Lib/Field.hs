module Lib.Field
( module Lib.Field
, Point, pattern Point
) where

import Lib ( both )
import Lib.Point ( Point, pattern Point, bounded )
import Control.Arrow ( (&&&) )
import Data.Vector ( Vector, (!?) )
import Data.Vector qualified as Vec

type Field a = Vector (Vector a)

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

extent :: Field a -> Point
extent = uncurry Point . both pred . (minimum . fmap Vec.length &&& Vec.length)

within :: Field a -> Point -> Bool
within field = bounded (extent field)
