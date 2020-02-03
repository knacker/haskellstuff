oder :: Bool -> Bool -> Bool
oder = (||)

nicht :: Bool -> Bool
nicht = not

--logik, siehe https://de.wikipedia.org/wiki/Wahrheitstabelle
und :: Bool -> Bool -> Bool
und a b = nicht(nicht a `oder` nicht b)

darausFolgt :: Bool -> Bool -> Bool
darausFolgt a b = (a `und` b) `oder` ((nicht a) `und` b) `oder` ((nicht a) `und` (nicht b))

genauDannwenn :: Bool -> Bool -> Bool
genauDannwenn a b = (a `und` b) `oder` ((nicht a) `und` (nicht b))

entwederOder :: Bool -> Bool -> Bool
entwederOder a b = ((nicht a) `und` b) `oder` (a `und` (nicht b))