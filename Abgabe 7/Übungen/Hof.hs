import Prelude hiding (map, any, all, iterate, filter, takeWhile, dropWhile, zipWith, curry, uncurry, (.), foldr, foldl, foldl1, foldr1, flip)

map :: (a->b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

any :: (a -> Bool) -> [a] -> Bool
any f [] = False
any f (x:xs) = if f x then True
               else any f xs 
               
all :: (a-> Bool) -> [a] -> Bool
all f [] = True
all f (x:xs) = if f x == False then False
               else all f xs
               
flip :: (a -> b -> c) -> b -> a -> c 
flip f xs a = f a xs 

iterate :: (a-> a) -> a -> [a]
iterate f x = f x : (iterate f (f x))

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f (x:xs) = if f x then  x : takeWhile f xs
                     else []
                     
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile f (x:xs) = if f x then dropWhile f xs
                     else x:xs
                     
filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x:xs) = if f x then x : (filter f xs) 
                  else filter f xs 
                  
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys 

(.) :: (b->c) -> (a->b) -> (a->c)
(.) f g = \x -> f (g x)

uncurry :: (a -> b -> c) -> (a,b) -> c 
uncurry f (a,b) = f a b

curry :: ((a,b) -> c) -> a -> b -> c 
curry f a b = f (a, b)

foldl :: (acc -> el -> acc) -> acc -> [el] -> acc 
foldl f acc [] = acc 
foldl f acc (x:xs) = foldl f (f acc x) xs 

foldr :: (el -> acc -> acc) -> acc -> [el] -> acc 
foldr f acc [] = acc
foldr f acc (x:xs) = foldr f (f x acc) xs
