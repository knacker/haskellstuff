import Data.Char

--1
kreuzprodukt :: [a] -> [b] -> [(a,b)]
kreuzprodukt (x:xs) (y:ys) = [ (x,y) | x <- (x:xs), y <- (y:ys)]

--2
geordnetePaare :: [Integer] -> [(Integer, Integer)]
geordnetePaare (xs) = [ (x,y) | x <- xs, y <- xs, x <= y]
 
--3
zusammenhaengen :: [[a]] -> [a]
zusammenhaengen xs = [x | y <- xs, x <- y]

zusammenhaengen2 :: [[a]] -> [a]
zusammenhaengen2 [] = []
zusammenhaengen2 (x:xs) = x ++ zusammenhaengen2 xs

--4 nimmt immer das zweite element aus dem tupel
zweite :: [(a,b)] -> [b]
zweite xs = [x | (a,b) <- xs, x <- [b]]

--ohne listcomprehensions
zweite2 :: [(a,b)] -> [b]
zweite2 [] = []
zweite2 ((a,b):xs) = [b] ++ zweite2 xs

--5 wenn durch 5 teilbar, wird sie zur liste hinzugefügt
durchFuenfTeilbar :: [Integer] -> [Integer]
durchFuenfTeilbar xs = [x | x <- xs, x `mod` 5 == 0]

--ohne listcomprehensions, selbe wie oben
durchFuenfTeilbar2 :: [Integer] -> [Integer]
durchFuenfTeilbar2 [] = []
durchFuenfTeilbar2 (x:xs) = if x `mod` 5 == 0 then [x] ++ durchFuenfTeilbar2 xs
                                              else durchFuenfTeilbar2 xs
                                             
--6 wenn n module x == 0, dann ist es ein teiler
teiler :: Integer -> [Integer]
teiler n = [x | x <- [1..n], mod n x == 0]

--ohne listcomprehensions
teiler2 :: Integer -> [Integer]
teiler2 0 = [0] --teiler von 0 ist 0
teiler2 n = sieb n n

sieb :: Integer -> Integer -> [Integer] --hilfsfunktion um die zahlen zu sieben
sieb n 1 = [1]
sieb n x = if (n `mod` x) == 0 then [x] ++ sieb n (x-1)
                               else sieb n (x-1)
                             
--7 wenn durch 1 und durch sich selbst teilbar, dann ist es eine primzahl
istPrim :: Integer -> Bool -- benutzt die teiler funktion um rauszufinden ob eine primzahl vorhanden ist
istPrim n = teiler n == [1,n]

--8 ruft funktion istprim auf um zu prüfen ob primzahl
primzahlen :: Integer -> [Integer]
primzahlen n = [x | x <- [2..n], istPrim x == True]

primzahlen2 :: Integer -> [Integer]
primzahlen2 0 = []
primzahlen2 n = if istPrim n == True then primzahlen2 (n-1) ++ [n]
                                     else primzahlen2 (n-1)
                                     
--9 tutorium
paare :: [a] -> [(a,a)]
paare [] = []
paare [x] = []
paare (x:xs) = [(x,(head xs))] ++ paare xs

--10 geht die liste durch und prüft ob x,y . wenn alle werte true sind, wird auch true zurückgegeben 
sortiert :: Ord a => [a] -> Bool
sortiert (x:xs) = and [x <= y | (x,y) <- paare (x:xs)]
                
sortiert2 :: Ord a => [a] -> Bool
sortiert2 [x] = True
sortiert2 (x:xs) = if x <= head xs then sortiert2 xs 
                                   else False

--11
anzKleinBuch :: String -> Int
anzKleinBuch xs = length [ x | x <- xs, isLower x] 

--wenn kleiner buchstabe, dann wird +1 gezählt und mit der restliste addiert
anzKleinBuch2 :: String -> Int
anzKleinBuch2 [] = 0
anzKleinBuch2 (x:xs) = if isLower x then counter 1 + anzKleinBuch2 xs
                                    else anzKleinBuch2 xs
--hilfsfunktion zum zählen
counter :: Int -> Int
counter n = n

--12
anzahl :: Char -> String -> Int
anzahl s xs = length[x | x <- xs, s == x]

--zählt mithilfe von counter die anzahl
anzahl2 :: Char -> String -> Int
anzahl2 _ [] = 0
anzahl2 x (y:ys) = if x == y then counter 1 +  anzahl2 x ys
                             else anzahl2 x ys
                             
--13
wiederhole :: Integer -> a -> [a]
wiederhole n y =  [y | x <-[1..n]]

--wiederholt solange bis 1 erreicht wurde, beendet dann rekursiven aufruf
wiederhole2 :: Integer -> a -> [a]
wiederhole2 1 y = [y]
wiederhole2 n y = wiederhole2 (n-1) y ++ [y]

--14 tutorium
istPerfekt :: Integer -> Bool
istPerfekt 0 = True
istPerfekt x = (sumEchteTeiler x == x)

sumEchteTeiler :: Integer -> Integer
sumEchteTeiler 0 = 0
sumEchteTeiler a = sum (teiler a) - a

perfekteZahlen :: Integer -> [Integer]
perfekteZahlen n = [x | x <- [1..n], istPerfekt x]

perfekteZahlen2 :: Integer -> [Integer]
perfekteZahlen2 0 = []
perfekteZahlen2 n = if istPerfekt n then n : perfekteZahlen2 (n-1)
                                    else perfekteZahlen2 (n-1)

--15 gibt die positionen aus
positionen :: Eq a => a -> [a] -> [Int]
positionen a [] = []
positionen a (x:xs) = hilfsfuktionPosi 0 a (x:xs)

hilfsfuktionPosi :: Eq a => Int -> a -> [a] -> [Int]    --hilfsfunktion zum zählen der positionen
hilfsfuktionPosi _ _ [] = []                                
hilfsfuktionPosi m n (x:xs) = if (n == x) then [m] ++ hilfsfuktionPosi (m + 1) n xs
                                          else hilfsfuktionPosi (m + 1) n xs
--zip nimmt ein tupel von je zwei elementen (1. liste die länge zählt, 2. eigentliche liste) und nimmt dann nur die elemten an denen n == element xs ist, zurückgegeben wird der index
positionen2 :: Eq a => a -> [a] -> [Int]
positionen2 n xs = [ i | (i, element) <- zip [1..] xs , n == element]