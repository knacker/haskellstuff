istElement :: Eq a => a -> [a] -> Bool
istElement n [] = False
istElement n (x:xs) = if n == x then True
                      else istElement n xs

istTeilmenge :: Eq a => [a] -> [a] -> Bool
istTeilmenge [] [] = True
istTeilmenge [] ys = True
istTeilmenge xs [] = False
istTeilmenge (x:xs) (y:ys) = if istElement x (y:ys) == False then False
                             else if x == y then istTeilmenge xs ys 
                             else istTeilmenge xs ys 
                             
istEchteTeilmenge :: Eq a => [a] -> [a] -> Bool
istEchteTeilmenge [] [] = False
istEchteTeilmenge [] ys = True
istEchteTeilmenge xs [] = False
istEchteTeilmenge (x:xs) (y:ys) = if istElement x (y:ys) == False then False
                             else if x == y then istEchteTeilmenge xs ys 
                             else istEchteTeilmenge xs ys                             