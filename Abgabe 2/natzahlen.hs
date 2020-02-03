--tutoriumsaufgabe
plus :: Integer -> Integer -> Integer
plus a b | b == 0 = a
         | a == 0 = b
         | otherwise = succ(plus (pred a) b)
--ruft mal rekursiv auf und addiert immer         
mal :: Integer -> Integer -> Integer
mal a b | b == 1 = a
        | a == 1 = b
        | b == 0 = 0
        | a == 0 = 0
        | otherwise = plus b (mal (pred a) b)

--ruft potenz immer wieder rekursiv auf, bis b 0 erreicht      
potenz :: Integer -> Integer -> Integer
potenz a b | b == 0 = 1
           | b == 1 = a
           | otherwise = mal (potenz a (pred b)) a