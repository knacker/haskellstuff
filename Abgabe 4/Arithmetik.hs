data Ausdruck = Konstante Integer
    | Variable String
    | Summe Ausdruck Ausdruck
    | Differenz Ausdruck Ausdruck
    | Produkt Ausdruck Ausdruck
    | Quotient Ausdruck Ausdruck
    deriving (Show)

--wertet die ausdrücke aus
ausdruckNachString :: Ausdruck -> String
ausdruckNachString (Konstante x) = show x
ausdruckNachString (Variable var) = var
ausdruckNachString (Summe x y) = "(" ++ ausdruckNachString x ++ " + " ++ ausdruckNachString y ++ ")"
ausdruckNachString (Differenz x y) = "(" ++ ausdruckNachString x ++ " - " ++ ausdruckNachString y ++ ")"
ausdruckNachString (Produkt x y) = "(" ++ ausdruckNachString x ++ " * " ++ ausdruckNachString y ++ ")"
ausdruckNachString (Quotient x y) = "(" ++ ausdruckNachString x ++ " / " ++ ausdruckNachString y ++ ")"

--prüft die belegung der variable, indem jeder tupel durchlaufen wird. wenn der ausdruck nicht vorhanden ist, ist man bei der leeren liste angelangt, was einen fehler auswirft
belegungVonVariable :: String -> [(String, Integer)] -> Integer
belegungVonVariable var [] = error "nicht definiert"
belegungVonVariable var ((a,b) : xs) = if var == a 
                                        then b
                                        else belegungVonVariable var xs

--wertet die ausdrücke wie bei ausdruck nach string aus                                        
auswerten :: Ausdruck -> [(String, Integer)] -> Integer
auswerten (Konstante k) ((a,b) : xs) = k
auswerten (Variable v) ((a,b) :xs) = belegungVonVariable v ((a,b):xs)
auswerten (Summe x y) ((a,b) : xs) = auswerten x ((a,b) : xs) + auswerten y ((a,b) : xs)
auswerten (Differenz x y) ((a,b) : xs) = auswerten x ((a,b) : xs) - auswerten y ((a,b) : xs)
auswerten (Produkt x y) ((a,b) : xs) = (auswerten x ((a,b) : xs)) * (auswerten y ((a,b) : xs))
auswerten (Quotient x y) ((a,b) : xs) = auswerten x ((a,b) : xs) `div` (auswerten y ((a,b) :  xs))