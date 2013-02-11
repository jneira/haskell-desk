module Monitor where
import Data.Maybe
import Data.Time
import System.Environment
import System.Process
import System.Exit
import System.CPUTime
import Control.Monad
import Control.Monad.LoopWhile
import Control.Concurrent

main= do (t:cmd:_) <- getArgs  
         id <- forkIO (forever.runCommand $ cmd)
         threadDelay $ 1000*read t
         killThread id

execAndRecover :: Int -> String -> IO ()
execAndRecover t cmd=
  do h <- runCommand cmd  
     t1 <-getTime
     exit <-waitFor h Nothing
     t2 <-getTime
     let td=t2-t1
     putStrLn $ "Proceso terminado con codigo "++show exit
     putStrLn $ "En "++(show td)++" milisegundos"
     when (td<(toInteger t)) $ do 
       putStrLn "Recuperando programa"
       verbose t cmd
     return ()

getTime=do t<-getCPUTime;return $ div t 1000000

waitFor handle Nothing=
  do exit<-getProcessExitCode handle
     waitFor handle exit
waitFor handle (Just exit) = return exit
     
verbose t cmd=  
  do tid <- forkIO (execAndRecover  t cmd)
     putStrLn $ "Ejecutando "++cmd++" con "++show tid
     threadDelay $ 1000*t
     killThread tid

