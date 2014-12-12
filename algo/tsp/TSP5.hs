-- from https://twitter.com/__josejuan__
module Main where
import Control.Monad 
import Data.Array.ST 
import Data.Array.Unboxed 
import Data.Bits 
import Data.List 
 
hkSolver :: UArray (Int, Int) Int -> Int -> [Int] 
hkSolver g n = findPath 1 $ head $ gen1Sets n n 
    where   sg = hkComputeSubpaths g n 
            -- no he visto como se hace este paso, pero supongo que será así.. 
            findPath k [] = [] 
            findPath k xs = k:findPath k' xs' 
                            where k' = fst $ head $ sortBy (\(_, a) (_, b) -> a `compare` b) $ 
                                       map (\j -> (j, sg!(si, j - 1) + g!(k, j))) xs' 
                                  si = gen1SetIndex 1 xs' 
                                  xs' = filter (/=k) xs 
 
-- algoritmo Held-Karp --- 
hkComputeSubpaths :: UArray (Int, Int) Int -> Int -> UArray (Int, Int) Int 
hkComputeSubpaths g n = runSTUArray $ do 
         a <- newArray ((0, 0), ((1 `shiftL` (n - 1)) - 1, n - 1)) 0 
         writeArray a (0, 0) 0 
         forM_ [2..n] $ \m -> 
            forM_ (gen1Sets n m) $ \s -> do 
                let sIndex = gen1SetIndex 1 s 
                forM_ (filter (/=1) s) $ \j -> do 
                    let jIndex = gen1SetIndex j s 
                        subCost k = readArray a (jIndex, k - 1) >>= return . (+ (g!(k, j))) 
                    minCost <- mapM subCost (filter (/=j) s) >>= return . minimum 
                    writeArray a (sIndex, j - 1) minCost 
         return a 
 
-- ================================================================================================ 
-- operaciones con sets q siempre contiene 1 ------------------------------------------------------ 
-- no es eficiente la representación de sets pero tampoco se usa tan intensivamente... 
-- combinaciones de n tomados de m en m pero siempre con 1 (no es eficiente) 
gen1Sets :: Int -> Int -> [[Int]] 
gen1Sets n m = filter ((==m).length) $ map (1:) $ g [2..n] 
    where g [] = [[]] 
          g (x:xs) = g' ++ map (x:) g' where g' = g xs 
-- dado un set de nºs genera un índice 
gen1SetIndex :: Int -> [Int] -> Int 
gen1SetIndex x = foldl (.|.) 0 . map (\k -> 1 `shiftL` (k - 2)) . filter (\k -> k /=1 && k /= x) 

-- grafos ----------------------------------------------------------------------------------------- 

noWay :: Int 
noWay = 1000000 
loadGraph :: [(Int, Int, Int)] -> UArray (Int, Int) Int 
loadGraph edges = array ((1, 1), (n, n)) es 
    where es = [edge a b | a <- [1..n], b <- [1..n]] 
          edges' = edges ++ map (\(a, b, c) -> (b, a, c)) edges 
          n = maximum $ map (\(a, _, _) -> a) edges' 
          edge a b = ((a, b), cost a b edges') 
          cost _ _ [] = noWay 
          cost a b ((c, d, e):ds) = if a == c && b == d then e else cost a b ds 


-- ejemplos --------------------------------------------------------------------------------------- 

sampleGraph = loadGraph [ (1, 2, 1) 
                        , (1, 3, 1) 
                        , (1, 5, 1) 
                        , (2, 3, 1) 
                        , (2, 4, 1) 
                        , (3, 4, 1) 
                        , (4, 6, 1) 
                        , (4, 7, 1) 
                        , (5, 7, 1) 
                        , (6, 7, 1) 
                        ] 

jneiraGraph :: [(Double, Double)] 
jneiraGraph = [(20833.3333, 17100.0000) 
              ,(20900.0000, 17066.6667) 
              ,(21300.0000, 13016.6667) 
              ,(21600.0000, 14150.0000) 
              --,(21600.0000, 14966.6667) 
              --,(21600.0000, 16500.0000) 
              ,(22183.3333, 13133.3333) 
              ,(22583.3333, 14300.0000) 
              ,(22683.3333, 12716.6667) 
              ,(23616.6667, 15866.6667) 
              ,(23700.0000, 15933.3333) 
              ,(23883.3333, 14533.3333) 
              ,(24166.6667, 13250.0000) 
              ,(25149.1667, 12365.8333) 
              ,(26133.3333, 14500.0000) 
              ,(26150.0000, 10550.0000) 
              ,(26283.3333, 12766.6667) 
              ,(26433.3333, 13433.3333) 
              ,(26550.0000, 13850.0000) 
              ,(26733.3333, 11683.3333) 
              ,(27026.1111, 13051.9444) 
              ,(27096.1111, 13415.8333) 
              ,(27153.6111, 13203.3333) 
              ,(27166.6667,  9833.3333) 
              ,(27233.3333, 10450.0000) 
              ] 

jneiraGraph' = loadGraph [(a + 1, b + 1, cost (j!!a) (j!!b)) | a <- [0..l], b <- [0..l], a /= b] 
    where j = jneiraGraph 
          l = length jneiraGraph - 1 
          cost (x, y) (x', y') = round $ sqrt ((x - x')^2 + (y - y')^2) 

main = print $ hkSolver jneiraGraph' $ length jneiraGraph 

