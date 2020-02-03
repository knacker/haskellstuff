andWithFoldr :: [Bool] -> Bool
andWithFoldr xs = foldr (&&) True xs 

andWithFoldl :: [Bool] -> Bool
andWithFoldl xs = foldl (&&) True xs 

orWithFoldr (x:xs) = foldr (||) x xs 

orWithFoldl (x:xs) = foldl (||) x xs 

sumWithFoldr (x:xs) = foldr (+) x xs 
sumWithFoldl (x:xs) = foldl (+) x xs 

productWithFoldr (x:xs) = foldr (*) x xs 

concatWithFoldr :: [[a]] -> [a]
concatWithFoldr (x:xs) = foldr (++) x xs 

concatWithFoldl :: [[a]] -> [a]
concatWithFoldl (x:xs) = foldl (++) x xs 

reverseWitrhFoldr :: [a] -> [a]
reverseWitrhFoldr [] = []
reverseWitrhFoldr (x:xs) = foldr (:) [x] (reverseWitrhFoldr xs)  

reverseWitrhFoldl :: [a] -> [a]
reverseWitrhFoldl [] = []
reverseWitrhFoldl xs = foldl (++) [(last xs)] [(reverseWitrhFoldl (init xs))]

mapWithFoldr :: (a->b) -> [a] -> [b]
mapWithFoldr f [] = []
mapWithFoldr f (x:xs) = foldr (++) [(f x)] [(mapWithFoldr f xs)] 

mapWithFoldl :: (a->b) -> [a] -> [b]
mapWithFoldl f [] = []
mapWithFoldl f (x:xs) = foldl (++) [(f x )] [(mapWithFoldl f xs )]

allWithFoldr :: (a-> Bool) -> [a] -> Bool
allWithFoldr f [] = True
allWithFoldr f (x:xs) = foldr (&&) (f x) [(allWithFoldr f xs )]

allWithFoldl f [] = True
allWithFoldl f (x:xs) = foldl (&&) (f x) [(allWithFoldl f xs)]

filterWithFoldr :: (a->Bool) -> [a] -> [a]
filterWithFoldr f [] = []
filterWithFoldr f (x:xs) = foldr (++) (if f x then [x] else []) [(filterWithFoldr f xs)] 

filterWithFoldl :: (a->Bool) -> [a] -> [a]
filterWithFoldl f [] = []
filterWithFoldl f (x:xs) = foldl (++) (if f x then [x] else []) [(filterWithFoldl f xs)]

maximumWithFoldr :: Ord a => [a] -> a 
maximumWithFoldr [x] = x
maximumWithFoldr (x:xs) = foldr max x xs

maximumWithFoldl [x] = x
maximumWithFoldl (x:xs) = foldl max x xs

minimumWithFoldr :: Ord a => [a] -> a
minimumWithFoldr (x:xs) = foldr min x xs 

minimumWithFoldr1 :: Ord a => [a] -> a 
minimumWithFoldr1 (x:xs) = foldr1 min (x:xs)

minimumWithFoldl1 (x:xs) = foldr1 min (x:xs)

appendWithFoldr :: [a] -> [a] -> [a]
appendWithFoldr [] ys = ys
appendWithFoldr xs [] = xs
appendWithFoldr [] [] = []
appendWithFoldr (x:xs) (y:ys) = foldr (++) ( [x] ++ [y]) [(appendWithFoldr xs ys)]

appendWithFoldl :: [a] -> [a] -> [a]
appendWithFoldl [] ys = ys
appendWithFoldl xs [] = xs
appendWithFoldl [] [] = []
appendWithFoldl (x:xs) (y:ys) = foldl (++) ([x] ++ [y]) [(appendWithFoldl xs ys)]

lengthWithFoldr :: [a] -> Int
lengthWithFoldr [] = 0
lengthWithFoldr (x:xs) = foldr (+) 1 [(lengthWithFoldr xs)]

lengthWithFoldl :: [a] -> Int
lengthWithFoldl [] = 0
lengthWithFoldl (x:xs) = foldl (+) 1 [(lengthWithFoldl xs)]