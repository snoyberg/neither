module Data.Neither.Class where

import Prelude hiding (either)
import qualified Data.Either as E
import Data.Neither.Base
import Control.Monad
import Control.Arrow ((&&&))

class Neither e where
  left :: a -> e a b
  right :: b -> e a b
  either :: (a -> c) -> (b -> c) -> e a b -> c

instance Neither Either where
  left = Left
  right = Right
  either = E.either

instance Neither MEither where
  left = MLeft
  right = MRight
  either = meither

instance Neither AEither where
  left = ALeft
  right = ARight
  either = aeither

mapLeft :: Neither e => (a -> c) -> e a b -> e c b
mapLeft = flip mapEither id

mapRight :: Neither e => (b -> c) -> e a b -> e a c
mapRight = mapEither id

mapEither :: Neither e => (a -> c) -> (b -> d) -> e a b -> e c d
mapEither f g = either (left . f) (right . g)

lefts :: (Neither e, MonadPlus m) => m (e a b) -> m a
lefts = (=<<) $ either return (const mzero)

rights :: (Neither e, MonadPlus m) => m (e a b) -> m b
rights = (=<<) $ either (const mzero) return

partitionEithers :: (Neither e, MonadPlus m) => m (e a b) -> (m a, m b)
partitionEithers = lefts &&& rights
