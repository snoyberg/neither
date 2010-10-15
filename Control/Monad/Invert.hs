{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Invert
    (
      -- * Typeclass
      MonadInvertIO (..)
      -- * Exceptions
    , finally
    , catch
    , block
    , unblock
    , bracket
    , bracket_
      -- * Memory allocation
    , alloca
    ) where

import Prelude hiding (catch)
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Error
import Control.Monad.Trans.State
import Control.Monad (liftM)
import qualified Control.Exception as E
import Data.Monoid (Monoid)
import qualified Foreign.Marshal.Alloc as A
import Foreign.Storable (Storable)
import Foreign.Ptr (Ptr)

class Monad m => MonadInvertIO m where
    data InvertedIO m :: * -> *
    type InvertedArg m
    invertIO :: m a -> InvertedArg m -> IO (InvertedIO m a)
    revertIO :: (InvertedArg m -> IO (InvertedIO m a)) -> m a

instance MonadInvertIO m => MonadInvertIO (IdentityT m) where
    newtype InvertedIO (IdentityT m) a =
        InvIdentIO { runInvIdentIO :: InvertedIO m a }
    type InvertedArg (IdentityT m) = InvertedArg m
    invertIO = liftM (fmap InvIdentIO) . invertIO . runIdentityT
    revertIO f = IdentityT $ revertIO $ liftM runInvIdentIO . f

instance (Error e, MonadInvertIO m) => MonadInvertIO (ErrorT e m) where
    newtype InvertedIO (ErrorT e m) a =
        InvErrorIO { runInvErrorIO :: InvertedIO m (Either e a) }
    type InvertedArg (ErrorT e m) = InvertedArg m
    invertIO = liftM (fmap InvErrorIO) . invertIO . runErrorT
    revertIO f = ErrorT $ revertIO $ liftM runInvErrorIO . f

instance MonadInvertIO m => MonadInvertIO (ReaderT r m) where
    newtype InvertedIO (ReaderT r m) a =
        InvReaderIO { runInvReaderIO :: InvertedIO m a }
    type InvertedArg (ReaderT r m) = (r, InvertedArg m)
    invertIO (ReaderT f) (r, arg) = liftM InvReaderIO $ invertIO (f r) arg
    revertIO f = ReaderT $ \r -> revertIO (\a -> liftM runInvReaderIO (f (r, a)))

instance (Monoid w, MonadInvertIO m) => MonadInvertIO (WriterT w m) where
    newtype InvertedIO (WriterT w m) a =
        InvWriterIO { runInvWriterIO :: InvertedIO m (a, w) }
    type InvertedArg (WriterT w m) = InvertedArg m
    invertIO = liftM (fmap InvWriterIO) . invertIO . runWriterT
    revertIO f = WriterT $ revertIO $ liftM runInvWriterIO . f

instance MonadInvertIO m => MonadInvertIO (StateT s m) where
    newtype InvertedIO (StateT s m) a =
        InvStateIO { runInvStateIO :: InvertedIO m (a, s) }
    type InvertedArg (StateT s m) = (s, InvertedArg m)
    invertIO (StateT f) (r, arg) = liftM InvStateIO $ invertIO (f r) arg
    revertIO f = StateT $ \r -> revertIO (\a -> liftM runInvStateIO (f (r, a)))

instance MonadInvertIO IO where
    newtype InvertedIO IO a = InvIO { runInvIO :: a }
    type InvertedArg IO = ()
    invertIO = const . liftM InvIO
    revertIO = liftM runInvIO . ($ ())

finally :: MonadInvertIO m => m a -> m b -> m a
finally action after =
    revertIO $ \a -> invertIO action a `E.finally` invertIO after a

catch :: (E.Exception e, MonadInvertIO m) => m a -> (e -> m a) -> m a
catch action handler =
    revertIO $ \a -> invertIO action a `E.catch` (\e -> invertIO (handler e) a)

block :: MonadInvertIO m => m a -> m a
block action = revertIO $ \a -> E.block $ invertIO action a

unblock :: MonadInvertIO m => m a -> m a
unblock action = revertIO $ \a -> E.unblock $ invertIO action a

bracket :: MonadInvertIO m
        => m a
        -> (a -> m b)
        -> (a -> m c)
        -> m c
bracket acquire cleanup action = revertIO $ \a -> E.bracket
    (invertIO acquire a)
    (\x -> invertIO (revertIO (const $ return x) >>= cleanup) a)
    (\x -> invertIO (revertIO (const $ return x) >>= action) a)

bracket_ :: MonadInvertIO m => m a -> m b -> m c -> m c
bracket_ acquire cleanup action = revertIO $ \a -> E.bracket_
    (invertIO acquire a)
    (invertIO cleanup a)
    (invertIO action a)

alloca :: (Storable a, MonadInvertIO m) => (Ptr a -> m b) -> m b
alloca f = revertIO $ \x -> alloca $ flip invertIO x . f
