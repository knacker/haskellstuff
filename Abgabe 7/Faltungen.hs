--3.1
andWithFoldr :: [Bool] -> Bool
andWithFoldr [] = error "leer"
andWithFoldr [x] = x
andWithFoldr xs = foldr (&&) True xs

andWithFoldl :: [Bool] -> Bool
andWithFoldl [] = error "leer"
andWithFoldl [x] = x
andWithFoldl xs = foldl (&&) True xs

--3.2
orWithFoldr :: [Bool] -> Bool
orWithFoldr [] = error "leer"
orWithFoldr [x] = x 
orWithFoldr xs = foldr (||) False xs

orWithFoldl :: [Bool] -> Bool
orWithFoldl [] = error "leer"
orWithFoldl [x] = x 
orWithFoldl xs = foldl (||) False xs

--3.3
sumWithFoldr :: (Num a) => [a] -> a 
sumWithFoldr [] = 0
sumWithFoldr [x] = x
sumWithFoldr xs = foldr (+) 0 xs

sumWithFoldl :: (Num a) => [a] -> a 
sumWithFoldl [] = 0
sumWithFoldl [x] = x
sumWithFoldl xs = foldl (+) 0 xs

--3.4
productWithFoldr :: (Num a) => [a] -> a
productWithFoldr [] = 0
productWithFoldr [x] = x
productWithFoldr xs = foldr (*) 1 xs

productWithFoldl :: (Num a) => [a] -> a
productWithFoldl [] = 0
productWithFoldl [x] = x
productWithFoldl xs = foldl (*) 1 xs
foldr (++) "y" ["g","a"]
--3.5 verkettet die listen in der liste
concatWithFoldr :: [[a]] -> [a] 
concatWithFoldr [] = []
concatWithFoldr (x:xs) = foldr (++) [] (x:xs)

concatWithFoldl :: [[a]] -> [a]
concatWithFoldl [] = []
concatWithFoldl (x:xs) = foldl (++) [] (x:xs)

--3.6 liste wird andersherum zurückgegeben
reverseWithFoldr :: [a] -> [a]
reverseWithFoldr [] = []
reverseWithFoldr xs = foldr (\y x -> x ++ [y]) [] xs

reverseWithFoldl :: [a] -> [a]
reverseWithFoldl [] = []
reverseWithFoldl xs = foldl (\y x -> [x] ++ y ) [] xs

--3.7 nimmt eine funktion und liste entgegen, und wendet auf jeden listenwert die funktion an
mapWithFoldr :: (a -> b) -> [a] -> [b]
mapWithFoldr f xs = foldr (\x y -> f x : y) [] xs

mapWithFoldl :: (a -> b) -> [a] -> [b]
mapWithFoldl f xs = foldl (\x y -> f y : x) [] xs

--3.8 erstellt eine liste von bools, die prüft ob true enthalten ist
anyWithFoldr :: (a -> Bool) -> [a] -> Bool
anyWithFoldr f xs = if True `elem` foldr (\x y -> f x : y) [] xs then True
                                                                 else False
                                                                 
anyWithFoldl :: (a -> Bool) -> [a] -> Bool
anyWithFoldl f xs = if True `elem` foldl (\x y -> f y : x) [] xs then True
                                                                 else False
                                                                 
--3.9 erstellt eine liste von bools, die prüft ob ein false enthalten ist
allWithFoldr :: (a -> Bool) -> [a] -> Bool
allWithFoldr f xs = if False `elem` foldr (\x y -> f x : y) [] xs then False
                                                                  else True
                                                                 
allWithFoldl :: (a -> Bool) -> [a] -> Bool
allWithFoldl f xs = if False `elem` foldl (\x y -> f y : x) [] xs then False
                                                                  else True
--3.10 filtert die elemente aus einer liste heraus, nimmt werte entgegen und wenn f x, dann wird es an die liste rangehängt, foldl vertauscht
filterWithFoldr :: (a -> Bool) -> [a] -> [a]
filterWithFoldr f = foldr (\x y -> if f x then x : y else y) []

filterWithFoldl :: (a -> Bool) -> [a] -> [a]
filterWithFoldl f = foldl (\x y -> if f y then y : x else x) []
--3.11 verwirft alle elemente, die nicht größer als der 1. wert sind
maximumWithFoldr1 :: Ord a => [a] -> a 
maximumWithFoldr1 xs = foldr1 (\x y -> if x > y then x else y) xs  

maximumWithFoldl1 :: Ord a => [a] -> a
maximumWithFoldl1 xs = foldl1 (\x y -> if y > x then y else x) xs   
                                                             
--3.12 verwirft alle werte, die kleiner als der 1. wert sind
minimumWithFoldr1 :: Ord a => [a] -> a
minimumWithFoldr1 xs = foldr1 (\x y -> if x < y then x else y) xs

minimumWithFoldl1 :: Ord a => [a] -> a
minimumWithFoldl1 xs = foldl1 (\x y -> if y < x then y else x) xs
--3.13 verkettet die beiden listen mit (:)
appendWithFoldr :: [a] -> [a] -> [a]
appendWithFoldr xs ys = foldr (:) xs ys

appendWithFoldl :: [a] -> [a] -> [a]
appendWithFoldl xs ys = foldl (flip (:)) xs ys

--3.14 summiert die foldr schritte auf
lengthWithFoldr :: [a] -> Int
lengthWithFoldr xs = foldr (\x n -> n + 1) 0 xs 

lengthWithFoldl :: [a] -> Int
lengthWithFoldl xs = foldl(\x n -> x + 1) 0 xs