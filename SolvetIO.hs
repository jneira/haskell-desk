module SolvetIO where
import System.Environment
import System.Directory
import System.Posix.Files
import Control.Monad


printDirs path=do fs <- getDirectoryContents path
                  dirs <- filterM doesDirectoryExist fs
                  print dirs
                  print $ length dirs
                  
printSize path=do fs <- getDirectoryContents path
                  files <- filterM doesFileExist fs
                  size  <-liftM sum $  mapM getFileSize files 
                  print (length files,size)

getFileSize= fileSize `fmap` getFileStatus
   
