--ruft bubblesort auf bis fertig sortiert wurde 
bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort [a] = [a]
bubbleSort xs = let ys = bubble xs
                in (bubbleSort (init ys)) ++ [last ys]

--bubble wird so lange aufgerufen, bis jedes element mit dem element dahinter 1 mal verglichen wurde
bubble :: Ord a => [a] -> [a]
bubble [a] = [a]
bubble [] = []
bubble (x:xs) = if (x < (head xs))  then x : bubble xs
                                    else (head xs) : bubble (x:(tail xs))

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
                               