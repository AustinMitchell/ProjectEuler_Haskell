main = print (largestFactor primes 600851475143 )

largestFactor :: [Integer] -> Integer -> Integer
largestFactor (p:ps) n
	| p*p > n 			= 1
	| n `mod` p == 0	= Main.max p (largestFactor ps n)
	| otherwise 		= largestFactor ps n

max :: Integer -> Integer -> Integer
max a b 
	| a > b 	= a
	|otherwise 	= b

-- Faster Eratos prime generation, drawn from http://www.justinshield.com/2011/05/haskell-a-better-prime-number-list-using-sieve-of-eratosthenes-and-lazy-pattern-matching/
primes :: [Integer]
primes = 2: 3: sieve (tail primes) [5,7..]  
 
sieve (p:ps) xs = h ++ sieve ps [x | x <- t, rem x p /=0]
  where (h,~(_:t)) = span(< p*p) xs