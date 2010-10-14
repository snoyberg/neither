{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
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

import Prelude hiding (either, catch)
import qualified Data.Either as E
import Control.Monad
import Control.Arrow ((&&&))
import Data.Monoid
import Control.Applicative
import Control.Failure
import Data.Typeable
import Data.Data
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Invert

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

data MEither a b = MLeft a | MRight b
    deriving (Typeable, Eq, Data, Ord, Read, Show)
instance Monad (MEither a) where
    return = MRight
    (MLeft a) >>= _ = MLeft a
    (MRight b) >>= f = f b
instance Functor (MEither a) where
    fmap = liftM
instance Applicative (MEither a) where
    pure = return
    (<*>) = ap
instance Failure e (MEither e) where
    failure = MLeft
meither :: (a -> c) -> (b -> c) -> MEither a b -> c
meither f _ (MLeft a) = f a
meither _ f (MRight b) = f b

data AEither a b = ALeft a | ARight b
    deriving (Typeable, Eq, Data, Ord, Read, Show)
instance Functor (AEither a) where
    fmap _ (ALeft a) = ALeft a
    fmap f (ARight b) = ARight $ f b
instance Monoid a => Applicative (AEither a) where
    pure = ARight
    ALeft x <*> ALeft y = ALeft $ x `mappend` y
    ALeft x <*> _ = ALeft x
    _ <*> ALeft y = ALeft y
    ARight x <*> ARight y = ARight $ x y
aeither :: (a -> c) -> (b -> c) -> AEither a b -> c
aeither f _ (ALeft a) = f a
aeither _ f (ARight b) = f b

newtype MEitherT e m a = MEitherT
    { runMEitherT :: m (MEither e a)
    }
mapMEitherT :: (m (MEither e a) -> n (MEither e' b))
             -> MEitherT e m a
             -> MEitherT e' n b
mapMEitherT f m = MEitherT $ f (runMEitherT m)

throwMEither :: Monad m => e -> MEitherT e m a
throwMEither = MEitherT . return . MLeft

instance Functor m => Functor (MEitherT e m) where
    fmap f = MEitherT . fmap (fmap f) . runMEitherT
instance (Functor m, Monad m) => Applicative (MEitherT e m) where
    pure = return
    (<*>) = ap
instance Monad m => Monad (MEitherT e m) where
    return = MEitherT . return . return
    (MEitherT x) >>= f = MEitherT $
        x >>= meither (return . MLeft) (runMEitherT . f)
instance Monad m => Failure e (MEitherT e m) where
    failure = MEitherT . return . MLeft
instance MonadTrans (MEitherT e) where
    lift = MEitherT . liftM MRight
instance MonadIO m => MonadIO (MEitherT e m) where
    liftIO = lift . liftIO

instance MonadInvertIO m => MonadInvertIO (MEitherT e m) where
    newtype InvertedIO (MEitherT e m) a =
        InvErrorIO { runInvErrorIO :: InvertedIO m (MEither e a) }
    type InvertedArg (MEitherT e m) = InvertedArg m
    invertIO = liftM (fmap InvErrorIO) . invertIO . runMEitherT
    revertIO f = MEitherT $ revertIO $ liftM runInvErrorIO . f
