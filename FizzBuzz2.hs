module FizzBuzz2 where

fizzbuzz x 
  | m 3 && m 5 || e '3' && e '5'= "fizzbuzz"
  | m 3 || e '3'= "fizz"
  | m 5 || e '5'= "buzz"
  | otherwise=show x
  where m d=mod x d==0
        e d=elem d $ show x
        
main =putStrLn $ unlines $ map fizzbuzz [1..100]
