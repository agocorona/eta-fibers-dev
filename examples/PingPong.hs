import Control.Concurrent.Fiber.Internal
import Control.Concurrent.Fiber.MVar
import Control.Monad.IO.Class
import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent(threadDelay)
import Control.Applicative
import Data.Monoid
import Control.Concurrent(myThreadId)
import System.IO.Unsafe
import Control.Exception (throw)
-- pingpong :: String -> MVar () -> MVar () -> Fiber ()
-- pingpong msg sourceChan sinkChan = go 0
--  where go n = do
--          takeMVar sourceChan
--          liftIO $ putStrLn $ show n ++ ": " ++ msg
--          putMVar sinkChan ()
--          go (n + 1)

--main :: IO ()
main = do
  forkFiber $ do 
     r2  <- empty <|> async (return " world2")
     th2 <- liftIO  myThreadId !> "MYTHREAD"
     liftIO $ print (r2,th2)

   `catchf` \Empty -> liftIO $ print "CATCHED EMPTY"
  threadDelay 2000000
   -- return ()
return1 s= threadDelay 1000 >> return s

evExit= unsafePerformIO $ MVar.newEmptyMVar


   -- pingChan <- newEmptyMVar
-- pongChan <- newEmptyMVar
-- forkFiber $ pingpong "Ping" pingChan pongChan
-- forkFiber $ pingpong "Pong" pongChan pingChan
-- -- Start the chain from the main thread!
-- MVar.putMVar pingChan ()
-- -- Wait 1 second
