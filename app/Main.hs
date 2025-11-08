module Main where

import ProducerConsumerChannel (mainProducerConsumerChannel)

-------------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "Hello, Haskell Main!"
  mainProducerConsumerChannel
  putStrLn "Goodbye, Haskell Main!"