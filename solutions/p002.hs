main = print(sum [x | x <- (genFib [1, 0] 4000000), mod x 2 == 0])

genFib :: [Integer] -> Integer -> [Integer]
genFib (x:xs) lim 
	| x + head(xs) <= lim = genFib ((x+head(xs)):x:xs) lim
	| otherwise = x:xs