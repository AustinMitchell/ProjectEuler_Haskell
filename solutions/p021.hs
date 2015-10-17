main = print (sum [x | x <- [1..9999], isAmicable x])

primeList = take 700 primes

isAmicable :: Integer -> Bool
isAmicable n
	| n == m 	= False
	| otherwise = sumProperDivisors m == n
		where m = sumProperDivisors n

sumProperDivisors :: Integer -> Integer
sumProperDivisors n = sum (tail (factors (primeFactors n) 1))

-- 	Given a list of prime factor/occurance pairs, generates a list of all the factors those prime
-- factors make
--		((x,nx):xs):	List of pairs
--			x:				Prime number
--			nx:				Number of occurances
--		n:				Current number the function is working with. Pass in 1 by default.
factors :: [(Integer, Integer)] -> Integer -> [Integer]
factors [] n			= [n]
factors ((x,nx):xs) n	= concatLists [factors (xs) (n*(x^(nx-i))) | i <- [0..nx]] 
	where 
		concatLists []		= []
		concatLists (x:xs) 	= x ++ (concatLists xs)

-- Generates a list of prime factor and number of occurance pairs for n
--		n:	Number to factor
primeFactors :: Integer -> [(Integer, Integer)]
primeFactors n = factorList n primeList [] False
	
-- Generates a list of prime factors for a given number, with number and amount of factors in each pair
--		n: 				Working number
--		(p:ps):			List of primes
--		(f:fs): 		Current generated list of prime factor and amount pairs
--		stillFactoring: Determines whether to make a new entry in (f:fs) or make a new one	
factorList :: Integer -> [Integer] -> [(Integer, Integer)] -> Bool -> [(Integer, Integer)]
factorList 0 _a _b _c = _b
factorList _a [] _b _c = _b
factorList n (p:ps) [] _
	| n `rem` p == 0 	= factorList (n `div` p) (p:ps) [(p, 1)] True
	| otherwise 		= factorList n ps [] False
factorList n (p:ps) ((fa,fb):fs) stillFactoring
	| n < p 			= ((fa,fb):fs)
	| n `rem` p == 0 	=
		if stillFactoring then 	factorList (n `div` p) (p:ps) ((fa, fb+1):fs) True
		else 					factorList (n `div` p) (p:ps) ((p, 1):(fa,fb):fs) True
	| otherwise			= factorList n ps ((fa,fb):fs) False

-- Faster Eratos prime generation, drawn from http://www.justinshield.com/2011/05/haskell-a-better-prime-number-list-using-sieve-of-eratosthenes-and-lazy-pattern-matching/
primes :: [Integer]
primes = 2: 3: sieve (tail primes) [5,7..]  
 
sieve (p:ps) xs = h ++ sieve ps [x | x <- t, rem x p /=0]
  where (h,~(_:t)) = span(< p*p) xs