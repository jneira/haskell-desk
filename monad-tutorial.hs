-- example 1
type Sheep = String

father :: Sheep -> Maybe Sheep
father s= Just $ "dad of " ++ s

mother :: Sheep -> Maybe Sheep
mother s= Just $ "mum of " ++ s 

test :: String -> Maybe Int
test _=Just 0
