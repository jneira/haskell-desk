module Main where
import Control.Monad
import Data.IORef

whileM :: (Monad m) => m Bool -> m a -> m ()
whileM cond body = do c <- cond
                      when c (body >> whileM cond body)

while :: Monad m => m Bool -> m () -> m () 
while test body = test >>= \b -> 
      if b then body >> while test body 
           else return ()

alpha ref = readIORef ref >>= \x-> 
      if x > 0 then print x >> writeIORef ref (x - 1) >> alpha ref 
      else return ()

alphad ref= do
       x <- readIORef ref
       if x > 0 then do print x
                        writeIORef ref (x-1)
                        alphad ref
                else return ()

alpha' ref = while ( fmap (>0) (readIORef ref) ) 
            (readIORef ref >>= print >> modifyIORef ref (subtract 1))