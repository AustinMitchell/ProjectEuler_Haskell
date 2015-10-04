main = print(lcdList [1..20])

gcd :: Integer -> Integer -> Integer
gcd a 0 = a
gcd a b = Main.gcd b $a `mod` b

lcd :: Integer -> Integer -> Integer
lcd a b = (a*b) `div` (Main.gcd a b)

lcdList :: [Integer] -> Integer
lcdList (a:b:[]) 	= Main.lcd a b
lcdList (x:xs) 		= Main.lcd x $lcdList xs