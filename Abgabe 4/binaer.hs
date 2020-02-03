data Bit = One | Zero deriving (Show)
type Bits = [Bit]

--normale umrechnungen für binärzahlen
integer2Bits :: Integer -> Bits
integer2Bits 1 = [One]
integer2Bits 0 = [Zero]
integer2Bits x = integer2Bits(div x 2) ++ integer2Bits(mod x 2)

bits2String :: Bits -> String
bits2String [One] = "1"
bits2String [Zero] = "0"
bits2String (x:xs) = bits2String[x] ++ bits2String xs

string2Bits :: String -> Bits
string2Bits "1" = [One]
string2Bits "0" = [Zero]
string2Bits (x:xs) = string2Bits [x] ++ string2Bits xs

integer2binString :: Integer -> String
integer2binString 1 = "1"
integer2binString 0 = "0"
integer2binString x = integer2binString(div x 2) ++ integer2binString(mod x 2)

--wandelt zunächst die bits in strings um und gibt diese an die "hilfsfunktion" weiter
bits2Integer :: Bits -> Integer
bits2Integer [One] = 1
bits2Integer [Zero] = 0
bits2Integer (x:xs) = bitString2Integer(bits2String(x:xs))

--berechnet nun die integer mit der normalen berechnunge von bits. 2^1 * 0 + 2^2 * 1 + ... anfangend bei der größten zahl
bitString2Integer :: String -> Integer
bitString2Integer "1" = 1
bitString2Integer "0" = 0
bitString2Integer (x:xs) = (2^(length(x:xs) - 1)) * bitString2Integer[x] + bitString2Integer xs

