--4.1)mergesort betrachtet eine liste, die in kleinere listen aufgeteilt wird, wo jede für sich sortiert wird
--4.2)ruft mergesort auf, wenn länge der liste größer n ist, wird mergesort angewandt, ansonsten selection sort
mergeSort :: Ord a => [a] -> Int ->[a]
mergeSort [] n = []
mergeSort [x] n = [x]
mergeSort xs n | length xs > n =  
                                 let 
                                  len = length xs
                                  i = div len 2
                                  (lower, upper) = splitAt i xs
                                 in merge (mergeSort lower n) (mergeSort upper n)
               
               | otherwise = insertionSort xs
               
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] bs = bs
merge cs [] = cs
merge (c:cs) (b:bs) = if c < b then c : merge cs (b:bs)
                               else b : merge (c:cs) bs

insertionSort :: Ord a => [a] -> [a]                               
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = if x < y then x:y:ys
                           else y:insert x ys
