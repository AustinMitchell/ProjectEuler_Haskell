main = print (sumPrimes primes 2000000)

sumPrimes :: [Integer] -> Integer -> Integer
sumPrimes (p:ps) n 
	| p >= n 	= 0
	| otherwise	= p + sumPrimes ps n

-- Faster Eratos prime generation, drawn from http://www.justinshield.com/2011/05/haskell-a-better-prime-number-list-using-sieve-of-eratosthenes-and-lazy-pattern-matching/
primes :: [Integer]
primes = 2: 3: sieve (tail primes) [5,7..]  
 
sieve (p:ps) xs = h ++ sieve ps [x | x <- t, rem x p /=0]
  where (h,~(_:t)) = span(< p*p) xs