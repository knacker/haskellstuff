type Gerade = (Punkt, Punkt)
type Punkt = (Double, Double)
data Gerade2 = Gerade2 {anstieg :: Double, offset :: Double} deriving Show

--berechnet y mit einsetzen von x
auswerten :: Gerade2 -> Double -> Double
auswerten (Gerade2 m n) x = m * x + n
--berechnet den schnittpunkt der 2 geraden, formel für schnittpunkt von https://de.wikipedia.org/wiki/Schnittpunkt
schnittpunkt :: Gerade -> Gerade -> Punkt
schnittpunkt ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) = ((((x4 - x3) * (x2*y1 - x1*y2) - (x2 - x1) *(x4*y3 - x3*y4)) / ((y4 - y3) * (x2 - x1) - (y2 - y1) * (x4 - x3))), (((y1 - y2) * (x4*y3 - x3*y4) - (y3 - y4) *(x2*y1 - x1*y2)) / ((y4 - y3) * (x2 - x1) - (y2 - y1) * (x4 - x3))))

--diese funktion zum berechnen benutzen    
flaecheZwischenGeraden :: Gerade -> Gerade -> Double -> Double -> Double
flaecheZwischenGeraden ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) i1 i2 = if i1 > sX && i2 > sX then flaecheZwischenGeraden2 ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) i1 i2
                                                                         else flaecheZwischenGeraden3 ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) i1 i2
    where (sX,sY) = schnittpunkt ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4))

--berechnet fläche für trapez          
flaecheZwischenGeraden2 :: Gerade -> Gerade -> Double -> Double -> Double
flaecheZwischenGeraden2 ((x1, y1), (x2, y2)) ((x3, y3) , (x4, y4)) i1 i2 =
    abs ((integrieren ((x1, y1) , (x2, y2)) i1 i2) - (integrieren ((x3, y3), (x4, y4)) i1 i2))
    
integrieren :: Gerade -> Double -> Double -> Double
integrieren ((x1, y1), (x2, y2)) i1 i2 = 
    abs ((m/2) * i1^2 + n * i1) - ((m/2) * i2^2 + n * i2)
    where 
    m = (y2 - y1) / (x2 - x1)
    n = -((m*x1) - y1)
    
--berechnet fläche für 2 dreiecke    
flaecheZwischenGeraden3 :: Gerade -> Gerade -> Double -> Double -> Double
flaecheZwischenGeraden3 ((x1, y1), (x2, y2)) ((x3, y3) , (x4, y4)) i1 i2 = f1 + f2
    where 
    (sX,sY) = schnittpunkt ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4))
    m1 = (y2 - y1) / (x2 - x1)
    n1 = -((m1*x1) - y1)
    m2 = (y4 - y3) / (x4 - x3)
    n2 = -((m2*x3) - y3)
    g1 = abs ((auswerten (Gerade2 m1 n1) i1) - (auswerten (Gerade2 m2 n2) i1))
    h1 = abs(sX - i1)
    g2 = abs ((auswerten (Gerade2 m1 n1) i2) - (auswerten (Gerade2 m2 n2) i2))
    h2 = abs (sX -i2)
    f1 = 1/2 * g1 * h1
    f2 = 1/2 * g2 * h2