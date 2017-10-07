module Control.Concurrent.Fiber.Internal where

import GHC.Base
import GHC.Conc.Sync
import Control.Monad
import Unsafe.Coerce

import Data.Typeable
import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Data.Monoid hiding (Any)
import Java.Core

-- Fiber

newtype Fiber a = Fiber { unFiber :: State# RealWorld -> (# State# RealWorld, a #) }

instance Functor Fiber where
  fmap f (Fiber io) = Fiber $ \s -> case io s of (# s1, a #) -> (# s1, f a #)


data Empty= Empty  deriving  (Show)
instance Exception Empty
  
instance Alternative Fiber where
  empty= throw Empty
  Fiber mf <|> Fiber mg=  Fiber $ mf `catch#` \Empty -> mg

catchf :: Exception e => Fiber a -> (e -> Fiber a) -> Fiber a
catchf (Fiber exp)  exc=  
    case IO exp `catch` (\e -> case exc e  of  Fiber r -> IO r) of
        (IO r) -> Fiber r

instance Monoid a => Monoid (Fiber a) where
  mempty= return mempty
  mappend x y=  (<>) <$> x <*> y 

instance Applicative Fiber where
  pure = return
  -- (<*>) = ap
  Fiber mf <*> Fiber mx = Fiber $ \s ->  
    case newMutVar# Nothing s  of
      (# s1, r1 #) -> case newMutVar# Nothing s1 of
        (# s2, r2 #)  -> 
          catch# (fparallel r1 r2 )     ( xparallel r1 r2 ) s2
  
      --   (State# RealWorld -> (# State# RealWorld, a #) )
      --  -> (b -> State# RealWorld -> (# State# RealWorld, a #) )
      --  -> State# RealWorld
      --  -> (# State# RealWorld, a #)
    where
    
    fparallel r1 r2 s=
        case mf s of
          (# s3, f #)  ->  case writeMutVar# r1  (Just f) s3  of
            s4#  -> case readMutVar# r2   s4#  of
              (# s5,mx #)  -> case mx of
                Just x  -> (# s5, f x #)
                Nothing -> raiseIO# (toException Empty) s5
                
    
    xparallel  r1 r2  (_ :: Empty) s=
        case mx s of
          (# s3, x #)  ->  case writeMutVar# r2 (Just x)  s3  of
            s4#  -> case readMutVar# r1   s4#  of
              (# s5,mf #)  -> case mf of
                Just f  -> (# s5, f x #)
                Nothing -> raiseIO# (toException Empty) s5
                


instance Monad Fiber where
  return :: a -> Fiber a
  return a = Fiber $ \s -> (# s, a #)

  (>>=) :: forall a b. Fiber a -> (a -> Fiber b) -> Fiber b
  (>>=) (Fiber m) f = Fiber $ \s ->
    case setCurrentC# (unsafeCoerce m) s of
      s1 -> case pushNextC# (unsafeCoerce f) s1 of
        s2 -> case m s2 of
          (# s3, a #) -> case popNextC# s3 of
            (# s4, _ #) ->
              case f a of
                fa -> case setCurrentC# (unsafeCoerce fa) s4 of
                  s5 -> unFiber fa s5

instance MonadIO Fiber where
  liftIO :: IO a -> Fiber a
  liftIO (IO m) = Fiber m

-- Fiber Utilities
runFiber :: forall a. Fiber a -> IO (Either (Fiber a) a)
runFiber (Fiber m) = undefined
  -- catch (fmap Right $ IO m) (\(Yield _ fiber) -> return $ Left (unsafeCoerce fiber))

runFiberWithBlock :: forall a. Fiber a -> IO (Either (Bool, Fiber a) a)
runFiberWithBlock (Fiber m) = undefined
--   catch (fmap Right $ IO m) $
--   \(Yield block fiber) -> return $ Left (block, unsafeCoerce fiber)

resumeFiber :: Fiber ()
resumeFiber = Fiber $ \s ->
  case getCurrentC# s of
    (# s1, fiber #) ->
      case (unsafeCoerce fiber) s1 of
        (# s2, a #) -> (# go a s2, () #)
  where go :: Any -> State# s -> State# s
        go a s =
          case popContStack# s of
            (# s1, 1#, cont1 #) ->
              let fa = (unsafeCoerce cont1) a
              in case setCurrentC# fa s1 of
                   s2 -> case fa s2 of
                     (# s3, a' #) -> go a' s3
            (# s1, _, _ #) -> s1



async :: IO a -> Fiber a 
async (IO io)=  Fiber $ \s -> io' s
        where
        unFiber (Fiber fib)= fib
        io' s =  case getEvent# s  of
          (# s2,0#, _ #) -> case io s2 of
                (# s3, x #) ->   case forkCont x s3  of
                    (# s5, _ #) ->  raiseIO# (toException Empty) s5
          
          (# s2, _, x #) -> case delEvent#  s2 of 
                      s3 -> (# s3, x #)

forkCont x= \s -> case getTSO# s of (#s1, tso #) -> fork# (execCont tso) s1
  where
  execCont tso =IO $ \s -> case setEvent#  (unsafeCoerce  x) s of
       s1 ->  case setContStack# tso s of s2 -> (unFiber resumeFiber) s2
  unFiber (Fiber fib)= fib
  
            

yield :: Fiber a
yield = yield' False

block :: Fiber a
block = yield' True

yield' :: Bool -> Fiber a
yield' block = Fiber $ \s ->
  case popContStack# s of
    (# s1, 1#, current #) ->
      let fa = (unsafeCoerce current) extractYieldError
      in case setCurrentC# (unsafeCoerce fa) s1 of
           s2 -> (# yieldFiber# (dataToTag# block) s2
                 ,  unreachableCodeError #)
    (# s1, _, _ #) -> (# s1, lastYieldError #)
  where extractYieldError =
          error "Attempted to extract a value from a Fiber's yield or block."
        lastYieldError =
          error "You cannot yield or block as the last action of a Fiber."
        unreachableCodeError =
          error "This code should not have been reached."

forkFiber :: Fiber () -> IO ThreadId
forkFiber (Fiber m)= IO $ \s ->
  case fork# m s of (# s1, tid #) -> (# s1, ThreadId tid #)

setEvent x= Fiber $ \s -> case setEvent# (unsafeCoerce  x) s of s1 -> (#s1 , () #)

getEvent :: Fiber (Maybe a)
getEvent = Fiber $ \s -> case getEvent#  s of
      (# s1, 1# , x #) -> (#s1, Just $ unsafeCoerce x #)
      (# s1,  _ , _ #) -> (#s1, Nothing #)

delEvent= Fiber $ \s -> case delEvent# s of  s1->  (# s1,() #)

-- Runtime primitives

data {-# CLASS "java.util.Stack" #-} Stack

type Stack# = Object# Stack

foreign import prim "eta.fibers.PrimOps.getCurrentC"
  getCurrentC# :: State# s -> (# State# s, Any #)

foreign import prim "eta.fibers.PrimOps.setCurrentC"
  setCurrentC# :: Any -> State# s -> State# s

foreign import prim "eta.fibers.PrimOps.pushNextC"
  pushNextC# :: Any -> State# s -> State# s

foreign import prim "eta.fibers.PrimOps.popNextC"
  popNextC# :: State# s -> (# State# s, Any #)

foreign import prim "eta.fibers.PrimOps.popContStack"
  popContStack# :: State# s -> (# State# s, Int#, Any #)

foreign import prim "eta.fibers.PrimOps.yieldFiber"
  yieldFiber# :: Int# -> State# s -> State# s

foreign import prim "eta.fibers.PrimOps.getEventCC"
  getEvent# :: State# s -> (# State# s, Int#, a #)

foreign import prim "eta.fibers.PrimOps.setEventC"
  setEvent# :: Any -> State# s -> State# s

foreign import prim "eta.fibers.PrimOps.delEventCC"
  delEvent# ::  State# s -> State# s

foreign import prim "eta.fibers.PrimOps.getTSOC"
   getTSO# ::  State# s -> (# State# s, ThreadId# #)

foreign import prim "eta.fibers.PrimOps.setConstStackC"
   setContStack# ::  ThreadId# -> State# s  -> State# s