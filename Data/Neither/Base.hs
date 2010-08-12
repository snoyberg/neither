{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Neither.Base where

import Control.Applicative
import Control.Monad
import Control.Failure
import Data.Typeable
import Data.Data
import Data.Monoid

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
