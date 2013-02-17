module Main where
import Control.Applicative
import Data.Monoid
import Data.List
import Data.List.Split
import Data.Ord
import Data.Ratio
import System.Environment

type Job=(Integer,Integer)

scheduleBy = sortBy
desc = flip
(<>) = mappend

--weightedCompletionTimes
wct :: [Job] -> [(Job,Integer)]
wct=snd.(mapAccumL f 0) 
  where f acc j@(w,l)=(l+acc,(j,(l+acc)*w))
sumwct=sum.(map snd).wct

solution1:: [Job] -> Integer
solution1=sumwct.schedule 
  where schedule=scheduleBy $ desc (diff <> weight)  
        diff=comparing $ uncurry (-) 
        weight=comparing fst

solution2=sumwct.schedule
  where schedule=scheduleBy $ desc (comparing ratio) 
        ratio (w,l)=w%l

readPair :: Read a => String -> (a,a)
readPair s= (w,l) 
  where [w,l]=map read $ splitOn " " s

jobsSolutions jobsFile= do
  s <-tail.lines <$> readFile jobsFile
  let jobs=map readPair s
  print $ solution1 jobs
  print $ solution2 jobs

main = do [jobsFile] <- getArgs
          jobsSolutions jobsFile
