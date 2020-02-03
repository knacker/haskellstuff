--vglFunktion :: el -> el -> Bool

quickSort :: (a -> a -> Bool) -> [a] -> [a]
quickSort _ [] = []
quickSort f (x:xs) = (quickSort f smaller) ++ [x] ++ (quickSort f larger)
                            where 
                                smaller = [y | y <- xs, f y x == False]
                                larger = [y | y <- xs, f y x == True]
                                
data Studiengang = Informatik | Ebusiness | IMT deriving (Show, Ord, Eq)
data Datum = Datum {tag::Int, monat::Int, jahr :: Int} deriving Show
data Student = Student {
    name :: String,
    vorname :: String,
    studiengang :: Studiengang,
    geburtsdatum :: Datum
    }
    deriving (Show)
--eingaben student1 = Student "Micha" "Ronny" Informatik (Datum 1 2 3)
--student2 = Student "Coleman" "Torben" Ebusiness (Datum 1 2 3)
--studentenListe = [student1, student2]
--quickSort nameVergleich studentenListe 

nameVergleich :: Student -> Student -> Bool
nameVergleich (Student name1 _ _ _) (Student name2 _ _ _) = if name1 >= name2  then True
                                                                               else False  
--vergleicht die beiden studiengänge                                                                               
vornameVergleich :: Student -> Student -> Bool
vornameVergleich (Student _ name1 _ _) (Student _ name2 _ _) = if name1 >= name2 then True
                                                                                 else False
--vergleicht die beiden studiengänge
studiengangVergleich :: Student -> Student -> Bool
studiengangVergleich (Student _ _ studi1 _) (Student _ _ studi2 _) = if studi1 >= studi2 then True 
                                                                                         else False
--vergleicht die daten, zuerst jahr, dann monat, dann tag                                                                                         
datumVergleich :: Student -> Student -> Bool
datumVergleich (Student _ _ _ (Datum a b c)) (Student _ _ _ (Datum d e f)) | c < f = True
                                                                           | c == f && b < e = True
                                                                           | c == f && b == e && a < d = True
                                                                           | otherwise = False
                                                                           
und :: (a -> a -> Bool) -> (a -> a -> Bool) -> (a -> a -> Bool)
und v1 v2 a1 a2 | (v1 a1 a2) && (v1 a2 a1) == False = v2 a1 a2
                | otherwise = v1 a1 a2