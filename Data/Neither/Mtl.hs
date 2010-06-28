{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Neither.Mtl () where

import Data.Neither.Base
import Prelude hiding (catch)
import Control.Monad
import "mtl" Control.Monad.Trans
import "MonadCatchIO-mtl" Control.Monad.CatchIO (MonadCatchIO (..))

instance MonadTrans (MEitherT e) where
    lift = MEitherT . liftM MRight
instance MonadIO m => MonadIO (MEitherT e m) where
    liftIO = lift . liftIO
instance MonadCatchIO m => MonadCatchIO (MEitherT e m) where
    m `catch` f = mapMEitherT (\m' -> m' `catch` \e -> runMEitherT $ f e) m
    block       = mapMEitherT block
    unblock     = mapMEitherT unblock
