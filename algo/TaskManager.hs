module Main where
import Data.List (mapAccumL)

type Bonus=Int
type Time=Int
type Task=(Bonus,Time)


viable :: [Task] -> Bool 
viable=undefined

schedule :: [Task] -> (Time,[Time]) 
schedule = mapAccumL addTime 0
  where addTime t (_,tt)=(t+tt,tt-t)

allOnTime :: [Task] -> Bool 
allOnTime=(all (>0)).snd.schedule 
--  where finishedBefore p n=

