-- STM Examples: Converting from MVar to STM
-- This file demonstrates various patterns for replacing MVars with STM

module STM_Examples where

import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (wait, withAsync)

-- Example 1: Simple MVar -> TMVar conversion
-- Original MVar pattern:
--   m <- newMVar value
--   withMVar m $ \val -> doSomething val

example1_MVar_to_TMVar :: IO ()
example1_MVar_to_TMVar = do
  -- Create TMVar instead of MVar
  counter <- newTMVarIO (0 :: Int)
  
  -- Instead of withMVar, use atomically with readTMVar or takeTMVar/putTMVar
  value <- atomically $ readTMVar counter  -- Like withMVar (non-destructive read)
  putStrLn $ "Counter value: " ++ show value
    
  -- Or if you need to modify:
  atomically $ do
    oldValue <- takeTMVar counter
    putTMVar counter (oldValue + 1)

-- Example 2: Multiple MVars -> Composable STM transactions
-- The power of STM: atomic operations on multiple variables
example2_composable_transactions :: IO ()
example2_composable_transactions = do
  account1 <- newTVarIO 100
  account2 <- newTVarIO 50
  
  -- Atomic transfer between accounts (impossible to do atomically with MVars!)
  atomically $ do
    bal1 <- readTVar account1
    bal2 <- readTVar account2
    
    if bal1 >= 30
      then do
        writeTVar account1 (bal1 - 30)
        writeTVar account2 (bal2 + 30)
      else retry  -- Automatically retry when condition isn't met

-- Example 3: Producer-Consumer with STM channels
-- Replacing MVar-based queues with STM channels
example3_producer_consumer :: IO ()
example3_producer_consumer = do
  queue <- newTBQueueIO 5  -- Bounded queue with capacity 5
  
  -- Producer
  withAsync (producer queue) $ \producerAsync -> do
    -- Consumer
    withAsync (consumer queue) $ \consumerAsync -> do
      wait producerAsync
      wait consumerAsync

producer :: TBQueue String -> IO ()
producer queue = do
  mapM_ (\i -> do
    atomically $ writeTBQueue queue ("Item " ++ show i)
    putStrLn $ "Produced: Item " ++ show i
    threadDelay 100000
    ) [1..10]
  -- Note: TBQueue doesn't have closeTBQueue, we'll just signal end differently
  atomically $ writeTBQueue queue "END"

consumer :: TBQueue String -> IO ()
consumer queue = consume
  where
    consume = do
      item <- atomically $ readTBQueue queue
      if item == "END"
        then putStrLn "Queue finished"
        else do
          putStrLn $ "Consumed: " ++ item
          threadDelay 150000
          consume

-- Example 4: STM with retry and orElse
-- These are powerful STM combinators not available with MVars
example4_retry_and_orElse :: IO ()
example4_retry_and_orElse = do
  resource1 <- newTVarIO False  -- Resource availability
  resource2 <- newTVarIO False
  
  -- Wait for either resource to become available
  result <- atomically $ 
    (do available1 <- readTVar resource1
        if available1 
          then return "Got resource 1"
          else retry)
    `orElse`
    (do available2 <- readTVar resource2
        if available2
          then return "Got resource 2" 
          else retry)
  
  putStrLn result

-- Example 5: STM data structures
-- Using TVar for mutable references (instead of IORef + MVar combinations)
data BankAccount = BankAccount
  { balance :: TVar Int
  , accountName :: String
  }

newAccount :: String -> Int -> STM BankAccount
newAccount name initialBalance = do
  bal <- newTVar initialBalance
  return $ BankAccount bal name

transfer :: BankAccount -> BankAccount -> Int -> STM ()
transfer from to amount = do
  fromBal <- readTVar (balance from)
  if fromBal >= amount
    then do
      writeTVar (balance from) (fromBal - amount)
      toBal <- readTVar (balance to)
      writeTVar (balance to) (toBal + amount)
    else retry

-- Key differences between MVar and STM:
--
-- 1. Composability: STM transactions can be composed atomically
--    - MVars: No way to atomically operate on multiple MVars
--    - STM: Multiple TVars can be modified in a single atomic transaction
--
-- 2. Blocking behavior:
--    - MVars: takeMVar blocks until available, putMVar blocks if full
--    - STM: retry blocks until conditions change, orElse provides alternatives
--
-- 3. Error handling:
--    - MVars: Exceptions can leave MVars in inconsistent states
--    - STM: Transactions are automatically rolled back on exceptions
--
-- 4. Deadlock prevention:
--    - MVars: Easy to create deadlocks with multiple MVars
--    - STM: No deadlocks possible due to optimistic concurrency
--
-- 5. Performance:
--    - MVars: Low overhead for simple cases
--    - STM: Higher overhead but scales better with complexity

-- When to use each:
-- - Use STM when you need composable transactions or complex coordination
-- - Use MVars for simple producer-consumer patterns with single variables
-- - Use STM when you might need to coordinate multiple resources atomically