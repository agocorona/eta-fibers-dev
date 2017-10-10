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
  
  keep $ do 
     r <- empty <|>  async ( liftIO (threadDelay 1000000) >>return " world") 
     --th2 <- liftIO  myThreadId !> "MYTHREAD"
     liftIO $ print r


mexit= unsafePerformIO $ MVar.newEmptyMVar  
keep mx= do
     forkFiber $ (mx  >> return ())  <|>  return ()
     MVar.takeMVar mexit   
   -- pingChan <- newEmptyMVar
-- pongChan <- newEmptyMVar
-- forkFiber $ pingpong "Ping" pingChan pongChan
-- forkFiber $ pingpong "Pong" pongChan pingChan
-- -- Start the chain from the main thread!
-- MVar.putMVar pingChan ()
-- -- Wait 1 second
