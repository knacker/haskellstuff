--durchläuft liste solange bis element gefunden wurde
istElement :: (Eq a) => a -> [a] -> Bool
istElement a [] = False
istElement a (x:xs) =  if a == x then True
                                 else istElement a xs
                                 
istTeilmenge :: (Eq a) => [a] -> [a] -> Bool
istTeilmenge (x:xs) [] = False
istTeilmenge (x:xs) (y:ys) = if istElement x (y:ys) == False then False -- wenn keine element der liste, false
                                                             else if xs == [] then True -- wenn xs leer ist, dann ist es eine teilmenge
                                                             else istTeilmenge xs (y:ys)

istEchteTeilmenge :: (Eq a) => [a] -> [a] -> Bool
istEchteTeilmenge [] [] = False -- wenn beide listen leer, sind die mengen gleich, also false
istEchteTeilmenge (x:xs) [] = False -- wenn die 1. liste größer als die 2., dann false
istEchteTeilmenge [] (x:xs) = True -- wenn die 1. liste leer ist und die 2. noch elemente enhält, ist es eine teilmenge
istEchteTeilmenge (x:xs) (y:ys) = if istElement x ys && x /= y then istEchteTeilmenge xs ys --wenn x nicht in y:ys enthalten ist, false, ansonsten wieder neu aufrufen
                                                     else False
--nimmt die menge xs und vereinigt diese mit der listcomprehension, in der alle elemente von y raus gefiltert werden die nicht in xs vorhanden sind                                            
vereinigung :: (Eq a) => [a] -> [a] -> [a]
vereinigung xs ys = xs ++ [ y |y <- ys, not(y `elem` xs)]
                                              
schnitt :: (Eq a) => [a] -> [a] -> [a]
schnitt (x:xs) [] = [] -- schnitt mit leerer menge ist leer
schnitt [] (x:xs) = []
schnitt (x:xs) ys = if istElement x ys then [x] ++ schnitt xs ys -- wenn x element von ys ist, wird x der liste hinzugefügt und der rest wieder aufgerufen
                                       else schnitt xs ys --wenn keine element, wird ohne x verknüpft
                                             