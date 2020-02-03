--prüft zunächst, ob die liste sortiert ist. wenn nicht, wird die tauschen funktion aufgerufen.
bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort [a] = [a]
bubbleSort xs = if istSortiert xs then xs
                                  else bubbleSort(tauschen xs)

--tauschen wird so lange aufgerufen, bis jedes element mit dem element dahinter 1 mal verglichen wurde
tauschen :: Ord a => [a] -> [a]
tauschen [a] = [a]
tauschen [] = []
tauschen (x:y:xs) = if (x <= y) then [x] ++ tauschen (y:xs)
                                else [y] ++ tauschen (x:xs)

--ist sortiert funktion prüft, ob eine liste sortiert ist und gibt dann einen boolean zurück
istSortiert :: Ord a => [a] -> Bool
istSortiert [x] = True
istSortiert (x:xs) = if x <= head xs then istSortiert xs 
                                     else False
--------------------------------------------------------------------------------------------
--selection sort prüft ob die liste sortiert ist, und wenn nicht wird das kleinste element an die liste vorn 
--rangehangen und selectionsort auf die restliste angewandt, aus der das kleinste element gelöscht wird
selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = (mind xs) : selectionSort (loesche (mind xs) xs)
--nimmt das kleinste element aus der liste heraus
mind :: Ord a => [a] -> a 
mind [a] = a
mind (x:xs) = if x <= head xs then mind (x:(tail xs))
                              else mind xs
--löscht das kleinste element beim ersten auftreten in der liste 
loesche :: Ord a => a -> [a] -> [a]
loesche el [] = []
loesche el (x:xs) = if el == x then xs
                               else [x] ++ (loesche el xs)
                               