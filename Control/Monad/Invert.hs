{-# LANGUAGE RankNTypes #-}
module Control.Monad.Invert
    ( -- * Typeclass
      MonadInvertIO (..)
      -- * Exceptions
    , finally
    , catch
    , block
    , unblock
    , bracket
    , bracket_
    , onException
      -- * Memory allocation
    , alloca
    , allocaBytes
    , withForeignPtr
    ) where

import Prelude hiding (catch)
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Error
import Control.Monad.Trans.State
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad (liftM)
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.RWS
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Exception as E
import Data.Monoid (Monoid)
import qualified Foreign.Marshal.Alloc as A
import Foreign.Storable (Storable)
import Foreign.Ptr (Ptr)
import Foreign.ForeignPtr (ForeignPtr)
import qualified Foreign.ForeignPtr as F
import Control.Monad.IO.Class (MonadIO)
import Data.Functor.Identity
import Data.Functor.Compose

newtype Flip f a b = Flip { getFlip :: f b a }

type Invert m n =
    forall a. (forall r. (forall b. n b -> m (r b)) -> m (r a)) -> n a

idInvert :: Monad m => Invert m m
idInvert f = liftM runIdentity $ f $ liftM Identity

composeInvert :: Monad m => Invert n o -> Invert m n -> Invert m o
(i `composeInvert` i') f =
    i $ \k -> i' $ \k' -> liftM getCompose $ f $ liftM Compose . k' . k

liftInvert :: (Monad m, Monad n) => Invert m n -> m a -> n a
liftInvert i m = i $ \k -> m >>= k . return

class MonadTransInvert t where
  invert :: Monad m => Invert m (t m)

class Monad m => MonadInvertIO m where
  invertIO :: Invert IO m

{-
class MonadIO m => MonadInvertIO m where
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
-}

finally :: MonadInvertIO m => m a -> m b -> m a
finally action after =
    invertIO $ \a -> a action `E.finally` a after

onException :: MonadInvertIO m => m a -> m b -> m a
onException action after =
    invertIO $ \a -> a action `E.onException` a after

catch :: (E.Exception e, MonadInvertIO m) => m a -> (e -> m a) -> m a
catch action handler =
    invertIO $ \a -> a action `E.catch` (\e -> a $ handler e)

block :: MonadInvertIO m => m a -> m a
block action = invertIO $ \a -> E.block $ a action

unblock :: MonadInvertIO m => m a -> m a
unblock action = invertIO $ \a -> E.unblock $ a action

bracket :: MonadInvertIO m
        => m a
        -> (a -> m b)
        -> (a -> m c)
        -> m c
bracket acquire cleanup action = invertIO $ \a -> E.bracket
    (a acquire)
    (\x -> a x)
    --(\x -> invertIO (revertIO (const $ return x) >>= cleanup) a)
    undefined
    -- (\x -> invertIO (revertIO (const $ return x) >>= action) a)

bracket_ :: MonadInvertIO m => m a -> m b -> m c -> m c
bracket_ acquire cleanup action = invertIO $ \a -> E.bracket_
    (a acquire)
    (a cleanup)
    (a action)

alloca :: (Storable a, MonadInvertIO m) => (Ptr a -> m b) -> m b
alloca f = invertIO $ \a -> A.alloca $ a . f

allocaBytes :: MonadInvertIO m => Int -> (Ptr a -> m b) -> m b
allocaBytes i f = invertIO $ \a -> A.allocaBytes i $ a . f

withForeignPtr :: MonadInvertIO m => ForeignPtr a -> (Ptr a -> m b) -> m b
withForeignPtr p f =
    invertIO $ \a -> F.withForeignPtr p $ a . f

instance MonadTransInvert IdentityT where
  invert f = IdentityT $ liftM runIdentity $ f $ \m -> liftM Identity $ runIdentityT m

instance MonadTransInvert ListT where
  invert f = ListT $ f $ \m -> runListT m

instance MonadTransInvert MaybeT where
  invert f = MaybeT $ f $ \m -> runMaybeT m

instance Error e => MonadTransInvert (ErrorT e) where
  invert f = ErrorT $ f $ \m -> runErrorT m

instance MonadTransInvert (ReaderT r) where
  invert f = ReaderT $ \r -> liftM runIdentity $ f $ \m -> liftM Identity $ runReaderT m r

instance MonadTransInvert (StateT s) where
  invert f = StateT $ \s -> liftM getFlip $ f $ \m -> liftM Flip $ runStateT m s
instance MonadTransInvert (Strict.StateT s) where
  invert f = Strict.StateT $ \s -> liftM getFlip $ f $ \m -> liftM Flip $ Strict.runStateT m s

instance Monoid w => MonadTransInvert (WriterT w) where
  invert f = WriterT $ liftM getFlip $ f $ \m -> liftM Flip $ runWriterT m
instance Monoid w => MonadTransInvert (Strict.WriterT w) where
  invert f = Strict.WriterT $ liftM getFlip $ f $ \m -> liftM Flip $ Strict.runWriterT m

newtype RWSTResult w s a = RWSTResult { unRWSTResult :: (a, s, w) }
instance Monoid w => MonadTransInvert (RWST r w s) where
  invert f = RWST $ \r s -> liftM unRWSTResult $ f $ \m -> liftM RWSTResult $ runRWST m r s
instance Monoid w => MonadTransInvert (Strict.RWST r w s) where
  invert f = Strict.RWST $ \r s -> liftM unRWSTResult $ f $ \m -> liftM RWSTResult $ Strict.runRWST m r s

instance MonadInvertIO m => MonadInvertIO (IdentityT m) where
  invertIO = invert `composeInvert` invertIO
instance MonadInvertIO m => MonadInvertIO (ListT m) where
  invertIO = invert `composeInvert` invertIO
instance MonadInvertIO m => MonadInvertIO (MaybeT m) where
  invertIO = invert `composeInvert` invertIO
instance (Error e, MonadInvertIO m) => MonadInvertIO (ErrorT e m) where
  invertIO = invert `composeInvert` invertIO
instance MonadInvertIO m => MonadInvertIO (ReaderT r m) where
  invertIO = invert `composeInvert` invertIO
instance MonadInvertIO m => MonadInvertIO (StateT s m) where
  invertIO = invert `composeInvert` invertIO
instance MonadInvertIO m => MonadInvertIO (Strict.StateT s m) where
  invertIO = invert `composeInvert` invertIO
instance (Monoid w, MonadInvertIO m) => MonadInvertIO (WriterT w m) where
  invertIO = invert `composeInvert` invertIO
instance (Monoid w, MonadInvertIO m) => MonadInvertIO (Strict.WriterT w m) where
  invertIO = invert `composeInvert` invertIO
instance (Monoid w, MonadInvertIO m) => MonadInvertIO (RWST r w s m) where
  invertIO = invert `composeInvert` invertIO
instance (Monoid w, MonadInvertIO m) => MonadInvertIO (Strict.RWST r w s m) where
  invertIO = invert `composeInvert` invertIO

instance MonadInvertIO IO where
  invertIO = idInvert
