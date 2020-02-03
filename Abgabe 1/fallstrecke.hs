--Aufgabe 3.1
fallstrecke :: Double -> Double
--Aufgabe 3.2, berechnet die fallstrecke
fallstrecke t = (1/2) * 9.81 * (t**2)
--Aufgabe 3.3
g :: Double
g = 9.81
--benutzt die variable g zum ausrechnen
fallstreckeGlobal :: Double -> Double
fallstreckeGlobal t = (1/2) * g * t ** 2
--Aufgabe 3.4, dasselbe wie oben, nur mit where
fallstreckeWhere :: Double -> Double
fallstreckeWhere t = (1/2) * gw * t**2	
	where gw = 9.81
--Aufgabe 3.5, gl wird in die formel eingefügt
fallstreckelet :: Double -> Double
fallstreckelet t = 
	let gl = 9.81
	in (1/2) * gl * t ** 2
--Aufgabe 3.6, wenn eingabe < 0, dann error
fallstreckeIf :: Double -> Double
fallstreckeIf t = 
	if t < 0 then error "Negative Zeit"
	else (1/2) * g * t**2 
		where g = 9.80665

--wenn eingabe kleiner 0, dann error. realisiert mit guards		
fallstreckeGuards :: Double -> Double
fallstreckeGuards t | t < 0 = error "negative Zeit"
					| otherwise = (1/2) * g * t ** 2
						where g = 9.80665
