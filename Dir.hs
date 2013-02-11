module Dir where 
import System.Directory 
import Control.Monad 

printDirs path=do fs <- getDirectoryContents path 
                  dirs <- filterM doesDirectoryExist fs 
                  print (dirs,length dirs) 

main= printDirs "."
