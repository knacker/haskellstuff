andWithFoldr :: [Bool] -> Bool
andWithFoldr xs = foldr (&&) True xs 

orWithFoldr xs = foldr (||) True xs

sumWithFoldr ::(Num a) => [a] -> a
sumWithFoldr [] = 0
sumWithFoldr (x:xs) = foldr (+) x ([sumWithFoldr xs])


productWithFoldr :: (Num a) => [a] -> a 
productWithFoldr (x:xs) = foldr (*) x xs

mapWithFoldr :: (a->b) -> [a] -> [b]
mapWithFoldr f [] = []
mapWithFoldr f (x:xs) = foldr (++) [(f x)] [(mapWithFoldr f xs)]

reverseWithFoldr :: [a] -> [a]
reverseWithFoldr [] = []
reverseWithFoldr (x:xs) = foldr (++) [x] [(reverseWithFoldr xs)]

anyWithFoldr :: (a -> Bool) -> [a] -> Bool
anyWithFoldr f [] = False
anyWithFoldr f (x:xs) = foldr (||) (f x) [(anyWithFoldr f xs)]