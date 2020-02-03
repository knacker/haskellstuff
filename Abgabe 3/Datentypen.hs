--Bruch :: Integer -> Integer -> Bruch
--zaehler :: Bruch -> Integer
--nenner :: Bruch -> Integer
data Bruch = Bruch {zaehler :: Integer, nenner :: Integer} deriving Show

data Hausaufgabe = Bestanden | Nichtbestanden | Nachbearbeitet  deriving Show

data Wochentag = Montag | Dienstag | Mittwoch | Donnerstag | Freitag | Samstag | Sonntag  deriving Show

data Monat = Januar | Februar | März | April | Mai | Juni | Juli | August | September | Oktober | November | Dezember deriving Show

--Datum :: Wochentag -> Monat -> Integer -> Datum
--tag :: Datum -> Wochentag
--monat :: Datum -> Monat
--jahr :: Datum -> Integer
data Datum = Datum{tag :: Wochentag, monat :: Monat, jahr :: Integer} deriving Show

--Uhrzeit :: Integer -> Integer -> Uhrzeit
--stunden :: Uhrzeit -> Integer
--minuten :: Uhrzeit -> Integer
data Uhrzeit = Uhrzeit {stunden :: Integer, minuten :: Integer} deriving Show

--Kasse :: Integer -> String -> Kasse
--kassenID :: Kasse -> Integer
--nachname :: Kasse -> String
data Kasse = Kasse {kassenID :: Integer, nachname :: String} deriving Show

--Preis :: Integer -> Integer -> Preis
--euro :: Preis -> Integer
--cent :: Preis -> Integer
data Preis = Preis {euro :: Integer, cent :: Integer} deriving Show

data Mensaessen = Tagessuppe | Sparessen | MensaVitalEssen | Bioessen | Vegetarisch | Aktion deriving Show

--Wahlessen :: Mensaessen -> Preis -> Wahlessen
--mensaessen :: Wahlessen -> Mensaessen
--preis :: Wahlessen -> Preis
data Wahlessen = Wahlessen{mensaessen :: Mensaessen, preis :: Preis}  deriving Show

--erstellt den kassenbon
--Kassenbon :: Kasse -> Datum -> Uhrzeit -> Wahlessen -> Kassenbon
--kasse :: Kassenbon -> Kasse
--datum :: Kassenbon -> Datum
--uhrzeit :: Kassenbon -> Uhrzeit
--essen :: Kassenbon -> Wahlessen
data Kassenbon = Kassenbon {kasse :: Kasse, datum :: Datum, uhrzeit :: Uhrzeit, essen :: Wahlessen} deriving Show
--eingabe  :  (Kassenbon (Kasse 3 "ingo") (Datum Dienstag April 1998) (Uhrzeit 23 21) (Wahlessen Tagessuppe (Preis 2 3)))

--Aufgabe f)
--gibt den kassierer vom kassenbon zurück
kassiererVonKassenbon :: Kassenbon -> String
kassiererVonKassenbon k = nachname(kasse k)
--gibt das jahr vom kassenbon zurück
jahrVomKassenbon :: Kassenbon -> Integer
jahrVomKassenbon j = jahr(datum j)
--gibt den preis vom kassenbon zurück
preisVomKassenbon :: Kassenbon -> Preis
preisVomKassenbon p = preis(essen p)
--gibt den tag vom kassenbon zurück
tagVomKassenbon :: Kassenbon -> Wochentag
tagVomKassenbon t = tag(datum t)

--Aufgabe e)
--führt die funktionen aus mit ausgabe
main = do
    print (Kassenbon (Kasse 3 "ingo") (Datum Dienstag April 1998) (Uhrzeit 23 21) (Wahlessen Tagessuppe (Preis 2 3)))
    print (kassiererVonKassenbon (Kassenbon (Kasse 3 "ingo") (Datum Dienstag April 1998) (Uhrzeit 23 21) (Wahlessen Tagessuppe (Preis 2 3))))
    print (jahrVomKassenbon (Kassenbon (Kasse 3 "ingo") (Datum Dienstag April 1998) (Uhrzeit 23 21) (Wahlessen Tagessuppe (Preis 2 3))))
    print (preisVomKassenbon (Kassenbon (Kasse 3 "ingo") (Datum Dienstag April 1998) (Uhrzeit 23 21) (Wahlessen Tagessuppe (Preis 2 3))))
    print (tagVomKassenbon (Kassenbon (Kasse 3 "ingo") (Datum Dienstag April 1998) (Uhrzeit 23 21) (Wahlessen Tagessuppe (Preis 2 3))))