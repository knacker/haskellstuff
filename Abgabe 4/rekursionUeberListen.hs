--wenn liste leer, true, ansonsten false
istLeer :: [a] -> Bool
istLeer [] = True
istLeer (x) = False

--nimmt die liste und hängt das y hinten ran
snoc :: [a] -> a -> [a]
snoc [] y = [y]
snoc (x:xs) y = x:xs ++ [y]

--ruft länge auf, bis die liste leer ist -> das ist dann die länge der liste
laenge :: [a] -> Integer
laenge [] = 0
laenge (x:xs) = laenge(xs) + 1

--nimmt das erste element aus der liste
erstesElement :: [a] -> a
erstesElement [] = error "leer"
erstesElement (x:xs) = x

--nimmt den rest der liste
rest :: [a] -> [a]
rest [x] = error "kein rest"
rest (x:xs) = xs

--nimmt das letzte element bis nur noch 1 drin ist
letztesElement :: [a] -> a
letztesElement [] = error "leer"
letztesElement [x] = x 
letztesElement (x:xs) = letztesElement xs

--nimmt solange anfang, bis die liste leer ist
anfang :: [a] -> [a]
anfang [] = error "leer"
anfang [x] = []
anfang (x:xs) = [x] ++ anfang xs 

--nimmt solange elemente aus der liste, bis 0 erreicht wurde
nimm :: Integer -> [a] -> [a]
nimm 0 (x:xs) = []
nimm n [] = []
nimm n (x:xs) = [x] ++ nimm (n-1) xs

--verwirft elemente bis 0 erreicht wurde
verwerfe :: Integer -> [a] -> [a]
verwerfe 0 (x:xs) = (x:xs)
verwerfe n (x:xs) = if laenge(x:xs) <= n 
                    then []
                    else verwerfe (n-1) xs

--summiert auf bis die liste leer ist
summe :: (Num a) => [a] -> a
summe [] = 0
summe (x:xs) = x + summe xs

--verdoppelt jedes element der liste bis die liste leer ist
verdopple :: (Num a) => [a] -> [a]
verdopple [] = []
verdopple (x:xs) = [x * 2] ++ verdopple xs

--fügt zwei listen zusammen
verkette :: [a] -> [a] -> [a]
verkette xs ys = xs ++ ys

--hängt das x immer hinten ran
rueckwaerts :: [a] -> [a]
rueckwaerts [] = []
rueckwaerts (x:xs) = rueckwaerts xs ++ [x]

--wenn x false, dann false, wenn liste leer wird true zurückgegeben
und :: [Bool] -> Bool
und [] = True
und (x:xs) = if (x == False) 
             then False
             else und xs
--wenn ein x true, wird true zurückgegeben, ansonsten false             
oder :: [Bool] -> Bool
oder [] = False
oder (x:xs) = if(x == True)
              then True
              else oder xs

--nimmt solange elemente aus der liste bis n und verwirft dann den rest der elemente ab n              
aufteilen :: Integer -> [a] -> ([a], [a])
aufteilen 0 xs = ([], xs)
aufteilen _ [] = ([], [])
aufteilen n (x:xs) = (nimm n (x:xs),verwerfe n(x:xs))

--wenn eine liste leer ist, wird leere liste zurückgegeben, ansonsten die beiden elemente verzahnt
verzahne :: [a] -> [b] -> [(a,b)]
verzahne [x] [] = []
verzahne [] [y] = []
verzahne [x] [y] = [(x,y)]
verzahne (x:xs) (y:ys) = [(x,y)] ++ verzahne xs ys

--nimmt eine liste, einen index und ein element und ersetzt das element am geforderten index. wenn n = 0 erreicht wurde, wird es ersetzt
aktualisiere :: [a] -> Integer -> a -> [a]
aktualisiere [] n el = [el]
aktualisiere (x:xs) 0 el = [el] ++ xs
aktualisiere (x:xs) n el = [x] ++ aktualisiere xs (n-1) el