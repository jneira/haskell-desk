module RandTree where
import Data.Tree
import System.Random

range=(0,2)
probChild:: StdGen -> [Int]
probChild g=randomRs range g

maxChilds=2
generator :: StdGen -> ([Int],[StdGen])
generator seed=
  let rs=take maxChilds $ probChild seed
      s=length $ filter (>0) rs
      childs=replicate s (snd.next $ seed)
  in (rs,childs)

genTree=unfoldTree generator
count=length.flatten

main= newStdGen >>= return.genTree

