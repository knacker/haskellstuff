--g)
--Punkt :: Double -> Double -> Punkt
data Punkt = Punkt Double Double deriving Show
--Bruch :: Integer -> Integer -> Bruch
data Bruch = Bruch Integer Integer deriving Show
--Datum :: String -> String -> Integer -> Datum
data Datum = Datum String String Integer deriving Show
--Uhrzeit :: Integer -> Integer -> Uhrzeit
data Uhrzeit = Uhrzeit Integer Integer deriving Show
--Kasse :: Integer -> String -> Kasse
data Kasse = Kasse Integer String deriving Show
--Kassenbonn :: Kasse -> Datum -> Uhrzeit -> Kassenbonn
data Kassenbonn = Kassenbonn Kasse Datum Uhrzeit deriving Show