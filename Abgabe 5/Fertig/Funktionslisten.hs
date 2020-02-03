resultat :: (Ord x) => [(x,y)] -> x -> y
resultat [] _ = error "nicht in Liste"
resultat ((x,y): rest) a = if (x == a) then y
                                       else resultat rest a
--wenn leere liste erreicht wurde, wird nothing ausgegeben, ansonste nur das y                                       
evtlResultat :: (Ord x) => [(x,y)] -> x -> Maybe y
evtlResultat [] _ = Nothing
evtlResultat ((x,y): rest) a = if (x == a) then (Just y)
                                           else evtlResultat rest a
--geht die liste durch und prüft ob das y mit dem eingegeben argument übereinstimmt, wenn ja wird es zur liste hinzugefügt                                           
urbilder :: (Ord y) => [(x,y)] -> y -> [x]
urbilder [] a = []
urbilder ((x,y) : rest) a = if ( y == a ) then [x] ++ urbilder rest a
                                          else urbilder rest a

--geht die liste durch und gibt die x aus                                          
echteArgumente :: (Ord x) => [(x,y)] -> [x]
echteArgumente [] = []
echteArgumente ((x,y) : rest) =  echteArgumente rest ++ [x]

--wenn x dem y entspricht, wird das x zur liste hinzugefügt
fixpunkte :: (Ord x) => [(x,x)] -> [x]
fixpunkte [] = []
fixpunkte ((x,y) : rest) = if (x == y) then  fixpunkte rest ++ [x]
                                       else fixpunkte rest
--wenn x = b, dann wird a mit y verknüpft                                       
funKomposition :: (Ord x) => [(x,x)] -> [(x,x)] -> [(x,x)]
funKomposition [] _ = []
funKomposition _ [] = []
funKomposition ((x,y) : xy) ((a,b) : ab) = if (x == b) then [(a, y)] ++ (funKomposition ((x,y):xy) ab)
                                                       else funKomposition ((x,y):xy) ab