module Marvel where
import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson (decode,Object)
import Network (withSocketsDo)
import Data.List (intersperse)
import Data.Hash.MD5
import Data.Time.Clock.POSIX (getPOSIXTime) 

publicKey= "8c893919b9be9291cced99b01cd370c4" 
privateKey= "ce0241c72edc7a3d70e656611172f697bf49c1bd"
hashedKey ts=md5s $ Str $  ts++privateKey++publicKey 
baseUrl= "http://gateway.marvel.com/v1/public"

url :: String -> [(String,String)] -> String
url rel [] = baseUrl ++ "/" ++ rel
url rel params = concat $ [baseUrl,"/",rel,"?"] ++ 
                 intersperse "&" [k++"="++v|(k,v)<-params]

apiParams :: String -> [(String,String)]
apiParams ts = [("apikey",publicKey),("hash",hashedKey ts),("ts",ts)] 
        
get :: String -> [(String,String)] -> IO (Maybe Object)
get rel params = do
    ts <- getPOSIXTime
    let ps=apiParams (show ts) ++ params
    json <- withSocketsDo $ simpleHttp (url rel ps) 
    return $ decode json

characters = get "characters" []
character i = get ("characters/" ++ show i) [] 


