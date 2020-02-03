--rechnet die volumen, hoehe, oberflaeche etc aus mit gegebener formel. gibt einen error aus, falls die eingabe negativ ist

volumenausRadiusUndHoehe :: Double -> Double -> Double
volumenausRadiusUndHoehe radius hoehe | radius < 0 = error "negative eingabe"
									  | hoehe < 0 = error "negative eingabe"
									  | otherwise = hoehe * pi * (radius * radius)

radiusAusVolumenUndHoehe :: Double -> Double -> Double
radiusAusVolumenUndHoehe volumen hoehe | volumen < 0 = error "negative eingabe"
							           | hoehe < 0 = error "negative eingabe"
                                       | otherwise = sqrt(volumen / (hoehe * pi))

hoeheAusVolumenUndRadius :: Double -> Double -> Double
hoeheAusVolumenUndRadius volumen radius | volumen < 0 = error "negative eingabe"
										| radius < 0 = error "negative eingabe"
                                        | otherwise = volumen / (radius*radius * pi)

oberflaecheAusRadiusUndHoehe :: Double -> Double -> Double
oberflaecheAusRadiusUndHoehe radius hoehe | radius < 0 = error "negative eingabe"
                                          | hoehe < 0 = error "negative eingabe"
                                          | otherwise = 2 * pi * radius *(radius + hoehe)

radiusAusOberflaecheUndHoehe :: Double -> Double -> Double
radiusAusOberflaecheUndHoehe oberflaeche hoehe | oberflaeche < 0 = error "negative eingabe"
                                               | hoehe < 0 = error "negative eingabe"
                                               | otherwise = -(hoehe / 2) + sqrt(((hoehe*hoehe) / 4) + (oberflaeche / (2 * pi))) 

hoeheAusOberflaecheUndRadius :: Double -> Double -> Double
hoeheAusOberflaecheUndRadius oberflaeche radius | oberflaeche < 0 = error "negative eingabe"
                                                | radius < 0 = error "negative eingabe"
                                                | otherwise = (oberflaeche / (2 * pi * radius)) - radius

volumenAusOberflaecheUndRadius :: Double -> Double -> Double
volumenAusOberflaecheUndRadius oberflaeche radius | oberflaeche < 0 = error "negative eingabe"
                                                  | radius < 0 = error "negative eingabe"
                                                  | otherwise = hoeheAusOberflaecheUndRadius oberflaeche radius * pi * (radius * radius)

volumenAusOberflaecheUndHoehe :: Double -> Double -> Double
volumenAusOberflaecheUndHoehe oberflaeche hoehe | oberflaeche < 0 = error "negative eingabe"
												| hoehe < 0 = error "negative eingabe"
                                                | otherwise = radiusAusOberflaecheUndHoehe oberflaeche hoehe * radiusAusOberflaecheUndHoehe oberflaeche hoehe * pi * hoehe

oberflaecheAusVolumenUndRadius :: Double -> Double -> Double
oberflaecheAusVolumenUndRadius volumen radius | volumen < 0 = error "negative eingabe"
											  | radius < 0 = error "negative eingabe"
											  | otherwise = 2 * pi * radius * (radius + ((volumen / ((radius * radius) * pi))))

oberflaecheAusVolumenUndHoehe :: Double -> Double -> Double
oberflaecheAusVolumenUndHoehe volumen hoehe | volumen < 0 = error "negative eingabe"
                                            | hoehe < 0 = error "negative eingabe"
                                            | otherwise = 2 * pi * sqrt(volumen / (hoehe * pi)) * (hoehe + (sqrt(volumen / (hoehe * pi))))