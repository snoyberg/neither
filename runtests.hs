{-# LANGUAGE DeriveDataTypeable #-}
import Prelude hiding (catch)

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Control.Monad.Invert
import Control.Exception (SomeException, Exception, throwIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef
import Data.Typeable (Typeable)

import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Error
import Control.Monad.Trans.State

throw :: (MonadIO m, Exception e) => e -> m a
throw = liftIO . throwIO

main :: IO ()
main = defaultMain
    [ testSuite "IdentityT" runIdentityT
    , testSuite "ReaderT" $ flip runReaderT "reader state"
    , testSuite "WriterT" runWriterT'
    , testSuite "ErrorT" runErrorT'
    , testSuite "StateT" $ flip evalStateT "state state"
    , testCase "ErrorT throwError" case_throwError
    , testCase "WriterT tell" case_tell
    ]
  where
    runWriterT' :: Functor m => WriterT [Int] m a -> m a
    runWriterT' = fmap fst . runWriterT
    runErrorT' :: Functor m => ErrorT String m () -> m ()
    runErrorT' = fmap (either (const ()) id) . runErrorT

testSuite :: (MonadIO m, MonadInvertIO m) => String -> (m () -> IO ()) -> Test
testSuite s run = testGroup s
    [ testCase "finally" $ case_finally run
    , testCase "catch" $ case_catch run
    -- FIXME test block and unblock
    , testCase "bracket" $ case_bracket run
    , testCase "bracket_" $ case_bracket_ run
    ]

ignore :: IO () -> IO ()
ignore x =
    catch x go
  where
    go :: SomeException -> IO ()
    go _ = return ()

data Exc = Exc
    deriving (Show, Typeable)
instance Exception Exc

one :: Int
one = 1

case_finally :: (MonadIO m, MonadInvertIO m) => (m () -> IO ()) -> Assertion
case_finally run = do
    i <- newIORef one
    ignore
        (run $ (do
            liftIO $ writeIORef i 2
            error "error") `finally` (liftIO $ writeIORef i 3))
    j <- readIORef i
    j @?= 3

case_catch :: (MonadIO m, MonadInvertIO m) => (m () -> IO ()) -> Assertion
case_catch run = do
    i <- newIORef one
    run $ (do
        liftIO $ writeIORef i 2
        throw Exc) `catch` (\Exc -> liftIO $ writeIORef i 3)
    j <- readIORef i
    j @?= 3

case_bracket :: (MonadIO m, MonadInvertIO m) => (m () -> IO ()) -> Assertion
case_bracket run = do
    i <- newIORef one
    ignore $ run $ bracket
        (liftIO $ writeIORef i 2)
        (\() -> liftIO $ writeIORef i 4)
        (\() -> liftIO $ writeIORef i 3)
    j <- readIORef i
    j @?= 4

case_bracket_ :: (MonadIO m, MonadInvertIO m) => (m () -> IO ()) -> Assertion
case_bracket_ run = do
    i <- newIORef one
    ignore $ run $ bracket_
        (liftIO $ writeIORef i 2)
        (liftIO $ writeIORef i 4)
        (liftIO $ writeIORef i 3)
    j <- readIORef i
    j @?= 4

case_throwError :: Assertion
case_throwError = do
    i <- newIORef one
    Left "throwError" <- runErrorT $
        (liftIO (writeIORef i 2) >> throwError "throwError")
        `finally`
        (liftIO $ writeIORef i 3)
    j <- readIORef i
    j @?= 3

case_tell :: Assertion
case_tell = do
    i <- newIORef one
    ((), w) <- runWriterT $ bracket_
        (liftIO (writeIORef i 2) >> tell [1])
        (liftIO (writeIORef i 4) >> tell [3])
        (liftIO (writeIORef i 3) >> tell [2])
    j <- readIORef i
    j @?= 4
    w @?= [2] -- FIXME should this be [1,2]?
