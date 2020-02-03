--rechnet den wochentag als zahl aus
robertson :: Integer -> Integer -> Integer -> Integer 
robertson tag monat jahr = (d + tag + 77 + e + ( b `quot` 400) - 2*( b `quot` 100)) `mod` 7 
    where a = monat + 10
          b = ((monat - 14) `quot` 12) + jahr
          c = a - 12*(a `quot` 13)
          d = (13* c - 1) `quot` 5
          e = (5 * (b `mod` 100) `quot` 4)

--gibt den wochentag zurück          
wochentag :: Integer -> String
wochentag tag 
    | tag == 1 = "Montag"
    | tag == 2 = "Dienstag"
    | tag == 3 = "Mittwoch"
    | tag == 4 = "Donnerstag"
    | tag == 5 = "Freitag"
    | tag == 6 = "Samstag"
    | otherwise = "Sonntag"