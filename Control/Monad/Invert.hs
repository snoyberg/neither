{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Invert
    ( MonadInvertIO (..)
    , finally
    , catch
    , block
    , unblock
    , bracket
    , bracket_
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

class Monad m => MonadInvertIO m where
    data InvertedIO m :: * -> *
    type InvertedArg m
    invertIO :: m a -> InvertedArg m -> IO (InvertedIO m a)
    revertIO :: (InvertedArg m -> IO (InvertedIO m a)) -> m a
    invertedBind :: (a -> m b) -> InvertedIO m a -> m b

instance MonadInvertIO m => MonadInvertIO (IdentityT m) where
    newtype InvertedIO (IdentityT m) a =
        InvIdentIO { runInvIdentIO :: InvertedIO m a }
    type InvertedArg (IdentityT m) = InvertedArg m
    invertIO = liftM (fmap InvIdentIO) . invertIO . runIdentityT
    revertIO f = IdentityT $ revertIO $ liftM runInvIdentIO . f
    invertedBind f (InvIdentIO x) =
        IdentityT $ invertedBind (runIdentityT . f) x

instance (Error e, MonadInvertIO m) => MonadInvertIO (ErrorT e m) where
    newtype InvertedIO (ErrorT e m) a =
        InvErrorIO { runInvErrorIO :: InvertedIO m (Either e a) }
    type InvertedArg (ErrorT e m) = InvertedArg m
    invertIO = liftM (fmap InvErrorIO) . invertIO . runErrorT
    revertIO f = ErrorT $ revertIO $ liftM runInvErrorIO . f
    invertedBind f (InvErrorIO x) =
        ErrorT $ invertedBind (runErrorT . g f) x
      where
        g :: Monad m => (a -> ErrorT e m b) -> Either e a -> ErrorT e m b
        g _ (Left e) = ErrorT $ return $ Left e
        g h (Right a) = h a

instance MonadInvertIO m => MonadInvertIO (ReaderT r m) where
    newtype InvertedIO (ReaderT r m) a =
        InvReaderIO { runInvReaderIO :: InvertedIO m a }
    type InvertedArg (ReaderT r m) = (r, InvertedArg m)
    invertIO (ReaderT f) (r, arg) = liftM InvReaderIO $ invertIO (f r) arg
    revertIO f = ReaderT $ \r -> revertIO (\a -> liftM runInvReaderIO (f (r, a)))
    invertedBind f (InvReaderIO x) =
        ReaderT $ \r -> invertedBind (flip runReaderT r . f) x

instance (Monoid w, MonadInvertIO m) => MonadInvertIO (WriterT w m) where
    newtype InvertedIO (WriterT w m) a =
        InvWriterIO { runInvWriterIO :: InvertedIO m (a, w) }
    type InvertedArg (WriterT w m) = InvertedArg m
    invertIO = liftM (fmap InvWriterIO) . invertIO . runWriterT
    revertIO f = WriterT $ revertIO $ liftM runInvWriterIO . f
    invertedBind f (InvWriterIO x) =
        WriterT $ invertedBind (runWriterT . g f) x
      where
        g h (a, w) = do
            tell w -- FIXME does this go second???
            b <- h a
            return b

instance MonadInvertIO m => MonadInvertIO (StateT s m) where
    newtype InvertedIO (StateT s m) a =
        InvStateIO { runInvStateIO :: InvertedIO m (a, s) }
    type InvertedArg (StateT s m) = (s, InvertedArg m)
    invertIO (StateT f) (r, arg) = liftM InvStateIO $ invertIO (f r) arg
    revertIO f = StateT $ \r -> revertIO (\a -> liftM runInvStateIO (f (r, a)))
    invertedBind f (InvStateIO x) =
        StateT $ \s -> invertedBind (flip runStateT s . g f) x -- something wrong here
      where
        g :: (a -> StateT s m b) -> (a, s) -> StateT s m b
        g h (a, sFIXME) = h a

instance MonadInvertIO IO where
    newtype InvertedIO IO a = InvIO { runInvIO :: a }
    type InvertedArg IO = ()
    invertIO = const . liftM InvIO
    revertIO = liftM runInvIO . ($ ())
    invertedBind f (InvIO a) = f a

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
    (\x -> invertIO (invertedBind cleanup x) a)
    (\x -> invertIO (invertedBind action x) a)

bracket_ :: MonadInvertIO m => m a -> m b -> m c -> m c
bracket_ acquire cleanup action = revertIO $ \a -> E.bracket_
    (invertIO acquire a)
    (invertIO cleanup a)
    (invertIO action a)
