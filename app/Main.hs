{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM

-------------------------------------------------------------------------------
main :: IO ()
main = do
  -- Using TMVar for the signal - allows multiple consumers
  signal <- newEmptyTMVarIO
  m <- newMVar ()

  putStrLn "Hello, Haskell!"

  let
    tasks =
      [ producer m signal
      , consumer "Consumer-1" m signal
      , consumer "Consumer-2" m signal
      , consumer "Consumer-3" m signal
      ]

  runConcurrently $ mconcat $ map Concurrently tasks

  -- withAsync (producer m signal) $ \a1 ->
  --   withAsync (consumer "Consumer-1" m signal) $ \a2 ->
  --     withAsync (consumer "Consumer-2" m signal) $ \a3 -> do
  --       withAsync (consumer "Consumer-3" m signal) $ \a4 -> do
  --         wait a1
  --         wait a2
  --         wait a3
  --         wait a4
  putStrLn "Goodbye, Haskell!"

-------------------------------------------------------------------------------
producer :: MVar () -> TMVar Int -> IO ()
producer m signal = do
  logm m "Producer: Starting work..."
  threadDelay 3_000_000 -- Simulate 3 seconds of work
  logm m "Producer: Work completed, notifying consumers..."

  -- Signal that work is done - this will wake up all waiting consumers
  emitSignal signal 77

-------------------------------------------------------------------------------
consumer :: String -> MVar () -> TMVar Int -> IO ()
consumer name m signal = do
  logm m $ name ++ ": Waiting for producer to finish..."

  -- Wait for the signal from producer
  v <- wakeOnSignal signal
  logm m $ name ++ ": Starting my work with value " ++ show v
  threadDelay 2_000_000 -- Simulate 2 seconds of consumer work
  logm m $ name ++ ": My work is done!"

-------------------------------------------------------------------------------
logm :: MVar () -> String -> IO ()
logm m message = withMVar m $ \_ -> putStrLn message

-------------------------------------------------------------------------------
emitSignal :: TMVar a -> a -> IO ()
emitSignal signal v =
  atomically $ putTMVar signal v

-------------------------------------------------------------------------------
wakeOnSignal :: TMVar a -> IO a
wakeOnSignal signal =
  -- readTMVar doesn't remove the value, so multiple consumers can read it
  atomically $ readTMVar signal
