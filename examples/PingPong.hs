import Control.Concurrent.Fiber.Internal
import Control.Concurrent.Fiber.MVar
import Control.Monad.IO.Class
import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent(threadDelay)
pingpong :: String -> MVar () -> MVar () -> Fiber ()
pingpong msg sourceChan sinkChan = go 0
 where go n = do
         takeMVar sourceChan
         liftIO $ putStrLn $ show n ++ ": " ++ msg
         putMVar sinkChan ()
         go (n + 1)

--main :: IO ()
main = do
  forkFiber $ do 
     r <- async (return "world")
     liftIO $ print r

   `catchf` \Empty -> return ()
-- pingChan <- newEmptyMVar
-- pongChan <- newEmptyMVar
-- forkFiber $ pingpong "Ping" pingChan pongChan
-- forkFiber $ pingpong "Pong" pongChan pingChan
-- -- Start the chain from the main thread!
-- MVar.putMVar pingChan ()
-- -- Wait 1 second
  threadDelay 1000000
-- return ()
