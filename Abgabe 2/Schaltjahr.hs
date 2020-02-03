--nur durch 4 teilbar oder nur durch 4 und 100 teilbar, ansonsten kein schaltjahr
schaltjahrIf :: Integer -> Bool
schaltjahrIf jahreszahl =
	if jahreszahl `mod` 100 == 0 && jahreszahl `mod` 400 == 0 && jahreszahl `mod` 4 == 0
	             then True 
				 else if jahreszahl `mod` 4 == 0 && jahreszahl `mod` 100 == 0
				     then False
					 else if jahreszahl `mod` 4 == 0
					     then True
						 else False

--bedingungen des gregoriaschen kalenders werden geprüft, genauso wie oben			  
schaltjahrGuards :: Integer -> Bool
schaltjahrGuards jahreszahl | jahreszahl `mod` 100 == 0 && jahreszahl `mod` 4 == 0 && jahreszahl `mod` 400 == 0 = True
                            | jahreszahl `mod` 100 == 0 && jahreszahl `mod` 4 == 0 = False
							| jahreszahl `mod` 4 == 0 = True
--schaltahr mit oder verknüpft							
schaltjahrBool :: Integer -> Bool
schaltjahrBool jahreszahl = (jahreszahl `mod` 100 == 0 && jahreszahl `mod` 4 == 0 && jahreszahl `mod` 400 == 0) || (jahreszahl `mod` 4 == 0 && jahreszahl `mod` 100 /= 0) 