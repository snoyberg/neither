{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PackageImports #-}
-- | This module provides three different datatypes: 'AEither' is the
-- applicative version of Either. It does not provide a monad instance, and
-- 'mappend's together error values. 'MEither' is the monadic version, which
-- only holds onto the first error value. 'MEitherT' is a monad transformer.
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
    ) where

import Prelude hiding (catch)
import Control.Applicative
import Control.Monad
import Data.Typeable
import Data.Data
import Data.Monoid
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import "MonadCatchIO-transformers" Control.Monad.CatchIO (MonadCatchIO (..))

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

instance Functor m => Functor (MEitherT e m) where
    fmap f = MEitherT . fmap (fmap f) . runMEitherT
instance Monad m => Monad (MEitherT e m) where
    return = MEitherT . return . return
    (MEitherT x) >>= f = MEitherT $
        x >>= meither (return . MLeft) (runMEitherT . f)
instance MonadTrans (MEitherT e) where
    lift = MEitherT . liftM MRight
instance MonadIO m => MonadIO (MEitherT e m) where
    liftIO = lift . liftIO
instance MonadCatchIO m => MonadCatchIO (MEitherT e m) where
    m `catch` f = mapMEitherT (\m' -> m' `catch` \e -> runMEitherT $ f e) m
    block       = mapMEitherT block
    unblock     = mapMEitherT unblock
