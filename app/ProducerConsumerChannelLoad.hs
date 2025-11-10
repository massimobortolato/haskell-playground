module ProducerConsumerChannelLoad (
  mainProducerConsumerChannelLoad,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Data.Foldable (for_)
import Data.Vector.Unboxed qualified as V
import Data.Word (Word8)

-------------------------------------------------------------------------------
data Payload = Payload
  { counter :: Int
  , width :: Int
  , height :: Int
  , channels :: Int
  , bytes :: V.Vector Word8
  }

-------------------------------------------------------------------------------
mainProducerConsumerChannelLoad :: IO ()
mainProducerConsumerChannelLoad = do
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
producer :: MVar () -> TChan (Maybe Payload) -> IO ()
producer m chan = do
  logm m "Producer: Starting work..."
  for_ [1 .. 5 :: Int] $ \i -> do
    threadDelay 3_000 -- Simulate 1 second of work
    logm m $ "Producer: Work completed, notifying consumers... " <> show i
    atomically $ do
      let
        width = 1000
        height = 1000
        channels = 3
      writeTChan chan $
        Just $
          Payload
            { counter = i
            , width = width
            , height = height
            , channels = channels
            , bytes = V.replicate (width * height * channels) 0
            }
  atomically $ writeTChan chan Nothing
  logm m "Producer: Finished work."

-------------------------------------------------------------------------------
consumer :: String -> MVar () -> TChan (Maybe Payload) -> IO ()
consumer name m bchan = do
  logm m $ name ++ ": Starting consumer..."
  chan <- atomically $ dupTChan bchan -- Create the duplicate channel once
  consumerLoop chan
 where
  consumerLoop :: TChan (Maybe Payload) -> IO ()
  consumerLoop chan = do
    logm m $ name ++ ": Waiting for message..."
    v <- atomically $ readTChan chan -- Read from the same duplicated channel
    case v of
      Nothing -> logm m $ name ++ ": Received termination signal. Exiting..."
      Just v' -> do
        logm m $ name ++ ": Received value: " ++ show v'
        -- printBytesAddress (bytes v')
        threadDelay 3_000
        consumerLoop chan

-------------------------------------------------------------------------------
logm :: MVar () -> String -> IO ()
logm m message = withMVar m $ \_ -> putStrLn message

-------------------------------------------------------------------------------
instance Show Payload where
  show (Payload counter width height channels bytes) =
    "Payload { counter: "
      ++ show counter
      ++ ", width: "
      ++ show width
      ++ ", height: "
      ++ show height
      ++ ", channels: "
      ++ show channels
      ++ ", bytes length: "
      ++ show (V.length bytes)
      ++ " }"

-------------------------------------------------------------------------------
-- printBytesAddress :: V.Vector Word8 -> IO ()
-- printBytesAddress vec = do
--   let (fptr, _offset, _len) = V.unsafeToForeignPtr vec
--   withForeignPtr fptr $ \ptr -> do
--     putStrLn $ "Address of bytes: " ++ show (castPtr ptr)
    -- hFlush stdout