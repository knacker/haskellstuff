--abcformel mit let in dargestellt
abcformelLet :: Double -> Double -> Double -> (Double, Double)
abcformelLet a b c = 
    let 
     r = 2 * a 
     s = sqrt(b**2 - 4 * a * c)
     x1 = (-b + s) / r
     x2 = (-b - s) / r
    in (x1, x2)
-- abcformel mit where dargestellt    
abcformelWhere :: Double -> Double -> Double -> (Double, Double)
abcformelWhere a b c = ((-b + s) / r, (-b - s) / r)
    where r = 2 * a
          s = sqrt(b**2 - 4 * a * c)