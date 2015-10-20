main = print (sum [(numArrangements 1 x numTileSlots) | x <- [2,3,4]])

numTileSlots = 50

fac :: Integer -> Integer
fac 0 = 1
fac 1 = 1
fac n = product [2..n]

-- Number of arrangements for tiles
--		n: Number of coloured tiles
--		s: Size of coloured tiles
-- 		m: Number of tile slots
numArrangements :: Integer -> Integer -> Integer -> Integer
numArrangements n s m
	| s*n > m 	= 0
	| otherwise	= (fac (m-n*(s-1))) `div` (fac (m-s*n) * fac n) + numArrangements (n+1) s m