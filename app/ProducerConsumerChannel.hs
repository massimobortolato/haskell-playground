module ProducerConsumerChannel (
  mainProducerConsumerChannel,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Data.Foldable (for_)

-------------------------------------------------------------------------------
mainProducerConsumerChannel :: IO ()
mainProducerConsumerChannel = do
  chan <- newBroadcastTChanIO
  m <- newMVar ()

  let
    tasks =
      [ producer m chan
      , consumer "Consumer-1" m chan
      , consumer "Consumer-2" m chan
      , consumer "Consumer-3" m chan
      ]

  runConcurrently $ mconcat $ map Concurrently tasks

-------------------------------------------------------------------------------
producer :: MVar () -> TChan (Maybe Int) -> IO ()
producer m chan = do
  logm m "Producer: Starting work..."
  for_ [1 .. 10 :: Int] $ \i -> do
    threadDelay 1_000_000 -- Simulate 1 second of work
    logm m $ "Producer: Work completed, notifying consumers... " <> show i
    atomically $ do
      writeTChan chan $ Just 71
      writeTChan chan $ Just 72
      writeTChan chan $ Just 73
  atomically $ writeTChan chan Nothing

-------------------------------------------------------------------------------
consumer :: String -> MVar () -> TChan (Maybe Int) -> IO ()
consumer name m bchan = do
  logm m $ name ++ ": Starting consumer..."
  chan <- atomically $ dupTChan bchan -- Create the duplicate channel once
  consumerLoop chan
 where
  consumerLoop :: TChan (Maybe Int) -> IO ()
  consumerLoop chan = do
    logm m $ name ++ ": Waiting for message..."
    v <- atomically $ readTChan chan -- Read from the same duplicated channel
    case v of
      Nothing -> logm m $ name ++ ": Received termination signal. Exiting..."
      Just v' -> do
        logm m $ name ++ ": Received value: " ++ show v'
        consumerLoop chan

-------------------------------------------------------------------------------
logm :: MVar () -> String -> IO ()
logm m message = withMVar m $ \_ -> putStrLn message