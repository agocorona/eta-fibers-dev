import Control.Concurrent.Fiber.Internal
import Control.Concurrent.Fiber.MVar
import Control.Monad.IO.Class
import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent(threadDelay,forkIO)
import Control.Applicative
import Data.Monoid
import Control.Concurrent(myThreadId)
import System.IO.Unsafe
import Control.Exception (throw)
import Data.IORef
-- pingpong :: String -> MVar () -> MVar () -> Fiber ()
-- pingpong msg sourceChan sinkChan = go 0
--  where go n = do
--          takeMVar sourceChan
--          liftIO $ putStrLn $ show n ++ ": " ++ msg
--          putMVar sinkChan ()
--          go (n + 1)

--main :: IO ()



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

-- main = do
  
--   keep $ do

    --  put[]
    --  setData "state data"
    --  r <- getData
    --  liftIO $ putStrLn r
    --  r <-   (async (return "hello")
    --      <> async (return " world") 
    --      <> async (return " world2"))  
    --      <|>  return "Alternative"
    --  --th2 <- liftIO  myThreadId !> "MYTHREAD"
    --  liftIO $ print r
main = do
      -- forkIO inputLoop
       forkIO reactLoop
       keep $ do
            r <-  (reactOption "hello")   <\>  (reactOption "world")
            liftIO $ print r 
            
            
       where
       reactOption :: String -> Fiber String
       reactOption s = do 
                x <- react setCallback (return ())
                if  x /= s then empty else do 
                   --   liftIO $ atomically $ writeTVar mvline ""
                      return s
         
       
       reactLoop =  do
                 x   <- getLine -- atomically $ readTVar mvline
                 mbs <- readIORef rcb
                 mapM (\cb -> cb x)  mbs
                 reactLoop
       
       rcb= unsafePerformIO $ newIORef [] 
       
       setCallback :: (String ->  IO ()) -> IO ()
       setCallback cb= atomicModifyIORef rcb $ \cbs ->  (reverse $ cb : cbs,())
       
       
