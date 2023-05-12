module Main (main) where

import Control.Concurrent
import BB84

main :: IO()
main = do
  (alice, bob) <- bb84 10
  printObservations alice
  printObservations bob
  let key = bb84Discard alice bob
  print key   

-- main :: IO ()
-- main = do
--   channel <- newChan

--   _ <- forkIO (threadA channel 10)
--   _ <- forkIO (threadB channel 10)

--   threadDelay 2000000

-- threadA :: Chan Int -> Int -> IO ()
-- threadA channel size = do
--   let vec = take size [1..]

--   mapM_ (writeChan channel) vec

-- threadB :: Chan Int -> Int -> IO ()
-- threadB channel size = do
--   vec <- loop size
--   print vec
--     where
--       loop sz
--         | sz == 0 = return []
--         | otherwise = do
--           e <- readChan channel
--           putStrLn $ "lido " ++ show e ++ "\n"
--           vec <- loop (sz - 1)
--           return $ e : vec