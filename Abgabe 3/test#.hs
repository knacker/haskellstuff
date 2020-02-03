data T = T String Integer deriving (Show)
data TListe = Leer | NichtLeer T TListe deriving (Show)
data VielleichtT = Nicht | Doch T deriving (Show)

loesche :: T -> TListe -> TListe
loesche a Leer = error "nicht da"
loesche (T b c) (NichtLeer (T d e) f )
    | (b == d) && (c == e) = f
    | otherwise = NichtLeer (T d e) (loesche (T b c) f )
ersetze :: T -> TListe -> TListe
ersetze g Leer = Leer
ersetze h@(T i _) (NichtLeer j@(T k _) l) = if i == k
    then NichtLeer h (ersetze h l)
    else NichtLeer j (ersetze h l)


findeAnIdx :: Integer -> TListe -> VielleichtT
findeAnIdx _ Leer = Nicht
findeAnIdx m (NichtLeer n o)
    | m == 0 = Doch n
    | otherwise = findeAnIdx ( pred m) o
    
liste1 = NichtLeer (T "Ernie" 7)
    (NichtLeer (T "Bert" 9)
        (NichtLeer (T "Ernie" 7) Leer))
liste2 = NichtLeer (T "Ernie" 8)
    (NichtLeer (T "Bibo" 7) Leer)
liste3 = NichtLeer (T "Bibo" 8)
    (NichtLeer (T "Bibo" 7) Leer)