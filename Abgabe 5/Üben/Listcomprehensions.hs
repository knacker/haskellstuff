import Data.Char

kreuzprodukt :: [a] -> [b] -> [(a,b)]
kreuzprodukt xs ys = [(x,y) | x <- xs, y <-ys]

geordnetePaare :: [Integer] -> [(Integer, Integer)]
geordnetePaare xs = [(x,y) | x <-xs, y <- xs, x <= y]

zusammenhaengen :: [[a]] -> [a]
zusammenhaengen xs = [ x | y <- xs, x <- y]

zweite :: [(a,b)] -> [b]
zweite xs = [ x | (a,b) <- xs, x <- [b]]

durchFuenfTeilbar :: [Integer] -> [Integer]
durchFuenfTeilbar xs = [ x  | x <- xs, mod x 5 == 0]

teiler :: Integer -> [Integer]
teiler n = [ x | x <- [1..n], mod n x == 0]

istPrim :: Integer -> Bool
istPrim n = if teiler n == [1, n] then True
            else False
            
primzahlen :: Integer -> [Integer]
primzahlen n = [ x | x <- [1..n], istPrim x]            

paare :: [a] -> [(a,a)]
paare [] = []
paare [x] = []
paare (x:xs) = [ (x,(head xs))] ++ paare xs   

sortiert :: Ord a => [a] -> Bool
sortiert xs = and [ x <= y | (x,y) <- paare xs] 

anzKleinBuch :: String -> Int 
anzKleinBuch [] = 0
anzKleinBuch xs = length[ x | x <- xs, isLower x]

anzahl :: Char -> String -> Int
anzahl a (x:xs) = length [b | b <- (x:xs), a == b]

wiederhole :: Integer -> a -> [a]
wiederhole n wert = [wert | y <- [1..n]]

istPerfekt :: Integer -> Bool
istPerfekt n = if sum [x | x <- [1..n], x <= n] == n then True else False