module Main where

import ProducerConsumerChannelLoad (mainProducerConsumerChannelLoad)

-------------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "**************"
  mainProducerConsumerChannelLoad
  putStrLn "##############"