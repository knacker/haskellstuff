module Menge( 
           Menge,
           leer,
           einfuegen,
           loeschen,
           vereinigung,
           schnitt,
           differenz,
           istLeer,
           istElement,
           istTeilmenge,
           istEchteTeilmenge,
           minimalesElement,
           maximalesElement
           ) where
             data Menge el = Menge [el] deriving (Eq)
             instance (Show el) => Show (Menge el) where 
               show (Menge liste) = "{" ++ init(tail(show liste)) ++ "}"
            
            --leer = leer  
             leer :: Menge el
             leer = Menge []
             
             -- einfuegen 3 (Menge [3,5,7])   
             einfuegen :: (Ord el) => el -> Menge el -> Menge el
             einfuegen el (Menge []) = Menge [el]
             einfuegen el (Menge (x:xs)) = Menge (el : (x:xs))
             --löscht, wenn elemente übereinstimmen
             loeschen :: (Ord el) => el -> Menge el -> Menge el
             loeschen el (Menge []) = Menge []
             loeschen el (Menge (x:xs)) = if el == x then Menge xs
                                                     else einfuegen x (loeschen el(Menge xs))
             --wenn x element von ys ist, wird rekursiv aufgerufen, ansonsten x in den rekursiven aufruf eingefügt                                       
             vereinigung :: (Ord el) => Menge el -> Menge el -> Menge el
             vereinigung (Menge []) (Menge xs) = Menge xs
             vereinigung (Menge xs) (Menge []) = Menge xs
             vereinigung (Menge (x:xs)) (Menge ys) = if istElement x (Menge ys) then vereinigung (Menge xs) (Menge ys)
                                                                                else einfuegen x (vereinigung (Menge xs) (Menge ys))
             --wenn x in ys ist, dann wird es eingefügt, ansonsten wird die funktion rekursiv aufgerufen bis eine menge leer ist
             schnitt :: (Ord el) => Menge el -> Menge el -> Menge el
             schnitt (Menge []) (Menge xs) = Menge []
             schnitt (Menge xs) (Menge []) = Menge []
             schnitt (Menge (x:xs)) (Menge ys) = if istElement x (Menge ys) then einfuegen x (schnitt (Menge xs) (Menge ys))
                                                                            else schnitt (Menge xs) (Menge ys)
             --A \ B 
             differenz :: (Ord el) => Menge el -> Menge el -> Menge el
             differenz (Menge []) (Menge xs) = Menge []
             differenz (Menge xs) (Menge []) = Menge xs
             differenz (Menge (x:xs)) (Menge ys) = if istElement x (Menge ys) then differenz (Menge xs) (Menge ys)
                                                                              else einfuegen x (differenz (Menge xs) (Menge ys))
             --prüft obl iste leer
             istLeer :: Menge el -> Bool
             istLeer (Menge []) = True
             istLeer (Menge xs) = False
             
             istElement :: (Ord el) => el -> Menge el -> Bool
             istElement el (Menge []) = False
             istElement el (Menge (x:xs)) = if el == x then True
                                                       else istElement el (Menge xs)
             --prüft, ob elemente in 2. liste vorhanden, wenn ja, solange prüfen bis erste leer ist                                           
             istTeilmenge :: (Ord el) => Menge el -> Menge el ->Bool
             istTeilmenge (Menge []) (Menge ys) = True
             istTeilmenge (Menge(x:xs)) (Menge []) = False
             istTeilmenge (Menge (x:xs)) (Menge (y:ys)) = if istElement x (Menge(y:ys)) == False then False -- wenn keine element der liste, false
                                                                         else istTeilmenge (Menge xs) (Menge (y:ys))
                                                                         
             istEchteTeilmenge :: (Ord a) => Menge a -> Menge a -> Bool
             istEchteTeilmenge (Menge[]) (Menge[]) = False -- wenn beide listen leer, sind die mengen gleich, also false
             istEchteTeilmenge (Menge(x:xs)) (Menge []) = False -- wenn die 1. liste größer als die 2., dann false
             istEchteTeilmenge (Menge[]) (Menge(x:xs)) = True -- wenn die 1. liste leer ist und die 2. noch elemente enhält, ist es eine teilmenge
             istEchteTeilmenge (Menge(x:xs)) (Menge(y:ys)) = if istElement x (Menge ys) && x /= y then istEchteTeilmenge (Menge xs) (Menge ys) --wenn x nicht in y:ys enthalten ist, false, ansonsten wieder neu aufrufen
                                                             else False
             --gibt das kleinste element der liste zurück                                               
             minimalesElement :: Menge el -> el
             minimalesElement (Menge []) = error "leer"
             minimalesElement (Menge [x]) = x
             minimalesElement (Menge (x:xs)) = minimalesElement (Menge xs)
             --gibt größtes element zurück
             maximalesElement :: Menge el -> el
             maximalesElement (Menge []) = error "leer"
             maximalesElement (Menge (x:xs)) = x
