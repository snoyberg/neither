{-# LANGUAGE DeriveDataTypeable #-}
import Prelude hiding (catch, either)

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Control.Exception (SomeException, Exception, throwIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef
import Data.Typeable (Typeable)

import Data.Neither
import Control.Exception.Peel
import Control.Monad.IO.Peel

main :: IO ()
main = defaultMain
    [ testSuite "MEitherT" runMEitherT'
    ]
  where
    runMEitherT' :: Functor m => MEitherT String m () -> m ()
    runMEitherT' = fmap (either (const ()) id) . runMEitherT

testSuite :: (MonadIO m, MonadPeelIO m) => String -> (m () -> IO ()) -> Test
testSuite s run = testGroup s
    [ testCase "finally" $ case_finally run
    , testCase "catch" $ case_catch run
    -- FIXME test block and unblock
    , testCase "bracket" $ case_bracket run
    , testCase "bracket_" $ case_bracket_ run
    , testCase "onException" $ case_onException run
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

case_finally :: (MonadIO m, MonadPeelIO m) => (m () -> IO ()) -> Assertion
case_finally run = do
    i <- newIORef one
    ignore
        (run $ (do
            liftIO $ writeIORef i 2
            error "error") `finally` (liftIO $ writeIORef i 3))
    j <- readIORef i
    j @?= 3

case_catch :: (MonadIO m, MonadPeelIO m) => (m () -> IO ()) -> Assertion
case_catch run = do
    i <- newIORef one
    run $ (do
        liftIO $ writeIORef i 2
        throw Exc) `catch` (\Exc -> liftIO $ writeIORef i 3)
    j <- readIORef i
    j @?= 3

case_bracket :: (MonadIO m, MonadPeelIO m) => (m () -> IO ()) -> Assertion
case_bracket run = do
    i <- newIORef one
    ignore $ run $ bracket
        (liftIO $ writeIORef i 2)
        (\() -> liftIO $ writeIORef i 4)
        (\() -> liftIO $ writeIORef i 3)
    j <- readIORef i
    j @?= 4

case_bracket_ :: (MonadIO m, MonadPeelIO m) => (m () -> IO ()) -> Assertion
case_bracket_ run = do
    i <- newIORef one
    ignore $ run $ bracket_
        (liftIO $ writeIORef i 2)
        (liftIO $ writeIORef i 4)
        (liftIO $ writeIORef i 3)
    j <- readIORef i
    j @?= 4

case_onException :: (MonadIO m, MonadPeelIO m) => (m () -> IO ()) -> Assertion
case_onException run = do
    i <- newIORef one
    ignore $ run $ onException
        (liftIO (writeIORef i 2) >> error "ignored")
        (liftIO $ writeIORef i 3)
    j <- readIORef i
    j @?= 3
    ignore $ run $ onException
        (liftIO $ writeIORef i 4)
        (liftIO $ writeIORef i 5)
    k <- readIORef i
    k @?= 4

case_throwMEither :: Assertion
case_throwMEither = do
    i <- newIORef one
    MLeft "throwMEither" <- runMEitherT $
        (liftIO (writeIORef i 2) >> throwMEither "throwMEither")
        `finally`
        (liftIO $ writeIORef i 3)
    j <- readIORef i
    j @?= 3
