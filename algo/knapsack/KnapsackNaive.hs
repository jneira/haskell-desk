module Main where

type Value=Int
type Size=Int
type Capacity=Int

solve:: Capacity -> [(Value,Size)] -> Value
solve c []=0
solve c ((v,s):xs)
 | s>c= solve c xs
 | otherwise= max (solve c xs) (v + solve (c-s) xs)
