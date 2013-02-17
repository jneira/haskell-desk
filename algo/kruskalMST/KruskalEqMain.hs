module Main where
import KruskalEq2
import System.Environment

main=do
  [file] <- getArgs
  solution file
