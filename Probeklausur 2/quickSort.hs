quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = let 
                s = smaller x xs 
                m = x : equal x xs
                l = larger x xs 
                in (quickSort s) ++ m ++ (quickSort l)
                
larger, smaller, equal :: (Ord a) => a -> [a] -> [a]
larger l [] = []
larger l (x:xs) = if x > l then x: (larger l xs)
                           else larger l xs

smaller s [] = []                          
smaller s (x:xs) = if x < s then x :(smaller s xs)
                            else smaller s xs 

equal e [] = []
equal e (x:xs) = if e == x then x : (equal e xs)
                           else equal e xs
-----------------------------------------------------------------                           
bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort [a] = [a]
bubbleSort xs = if istSortiert xs then xs
                                  else bubbleSort(bubble xs)
bubble :: (Ord a) =>[a] -> [a]
bubble [] = []
bubble [x] = [x]
bubble (x:y:xs) = if x < y then x : bubble (y:xs)
                           else y : bubble (x:xs)

istSortiert :: (Ord a) => [a] -> Bool
istSortiert [] = True
istSortiert [x] = True
istSortiert (x:xs) = if x <= head xs then istSortiert xs
                                     else False  