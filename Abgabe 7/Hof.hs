import Prelude hiding (map, any, all, iterate, filter, takeWhile, dropWhile, zipWith, curry, uncurry, (.), foldr, foldl, foldl1, foldr1, flip)

--2.1 
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = (f x) : (map f xs)

--2.2 wenn ein x in der liste true ist, dann true. liste leer = false
any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any f (x:xs) = (f x) || (any f xs)

--2.3 wenn alle f x true, dann true 
all :: (a -> Bool) -> [a] -> Bool
all f [] = True
all f (x:xs) = if (f x) == True then (all f xs)
                                else False
                                
--2.4
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x
 
--2.5
iterate :: (a -> a) -> a -> [a]
iterate f x = (f x) : (iterate f (f x))

--2.6 nimmt solange elemente, bis die bedingung erfüllt ist 
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f [] = []
takeWhile f (x:xs) = if (f x) == True then x : (takeWhile f xs)
                                      else []
                                      
--2.7 sobald bedingung true, werden alle elemente danach rausgeworfen
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile f [] = []
dropWhile f (x:xs) = if (f x) == True then dropWhile f xs
                                      else (x:xs)
                                      
--2.8 wenn true, dann wird element angehängt und liste rekusirv aufgerufen
filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x:xs) = if (f x) == False then filter f xs
                                    else x : (filter f xs)

--hilfsfunktion für partition
filter2 :: (a -> Bool) -> [a] -> [a]
filter2 f [] = []
filter2 f (x:xs) = if (f x) == True then filter2 f xs
                                    else x : (filter2 f xs)
                               
--2.9 filtert beide listenseiten, für die eine true, andere false 
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f xs = (filter f xs, filter2 f xs)

--2.10 wendet funktion auf beide listenelemente an
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] _ = []
zipWith f _ [] = []
zipWith f (x:xs) (y:ys) = (f x y) : (zipWith f xs ys) 

--2.11 verkettet die beiden funktionen
(.) :: (b -> c) -> ( a -> b) -> (a -> c)
(.) f g = \a -> f (g a)

--2.12 macht aus einem tupel 2 werte
uncurry :: (a->b-> c) -> (a,b) -> c
uncurry f = \(a,b) -> f a b

--2.13 macht aus 2 werten ein tupel 
curry :: ((a,b) -> c) -> a -> b -> c 
curry f = \a b -> f (a,b)

--2.14 faltet von rechts die funktion
foldl :: (acc -> el -> acc) -> acc -> [el] -> acc
foldl f a [] = a
foldl f a (x:xs) = foldl f(f a x) xs

--2.15 faltet von links die funktion
foldr :: (el -> acc -> acc) -> acc -> [el] -> acc
foldr f a [] = a
foldr f a (x:xs) = f x (foldr f a xs)

--Zusatz : 