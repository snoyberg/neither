-- | This module provides three different datatypes: 'AEither' is the
-- applicative version of Either. It does not provide a monad instance, and
-- 'mappend's together error values. 'MEither' is the monadic version, which
-- only holds onto the first error value. 'MEitherT' is a monad transformer.
--
-- Also, *Either datatypes and utility functions from Data.Either
-- are generalized with 'Neither' type class.

module Data.Neither
    ( -- * Applicative version
      AEither (..)
    , aeither
      -- * Monadic version
    , MEither (..)
    , meither
      -- * Monad transformer
    , MEitherT (..)
    , mapMEitherT
    , throwMEither
      -- * Neither typeclass
    , Neither (..)
      -- * Utility functions
    , mapLeft
    , mapRight
    , mapEither
    , lefts
    , rights
    , partitionEithers
    ) where

import Data.Neither.Base
import Data.Neither.Class
import Data.Neither.Transformers ()
import Data.Neither.Mtl ()
