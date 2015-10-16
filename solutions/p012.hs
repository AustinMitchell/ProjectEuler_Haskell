import Data.Set (Set)
import qualified Data.Set as Set

main = print (Set.fromList (take 10 (primeFactors 0 0)))

primeList = take 500 primes

-- Generates a list of prime factors for triangle numbers, always call 0 0 first
--		i: iteration
-- 		n: current triangle number
primeFactors :: Integer -> Integer -> [(Integer, [(Integer, Integer)])]
primeFactors 0 0 = (3, [(3, 1)]) : primeFactors 4 6
primeFactors i n = (n, factorList n primeList [] False) : (primeFactors (i+1) (n+i))
	
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
	| n `rem` p == 0 	=
		if stillFactoring then 	factorList (n `div` p) (p:ps) ((fa, fb+1):fs) True
		else 					factorList (n `div` p) (p:ps) ((p, 1):(fa,fb):fs) True
	| otherwise			= factorList n ps ((fa,fb):fs) False

-- Faster Eratos prime generation, drawn from http://www.justinshield.com/2011/05/haskell-a-better-prime-number-list-using-sieve-of-eratosthenes-and-lazy-pattern-matching/
primes :: [Integer]
primes = 2: 3: sieve (tail primes) [5,7..]  
 
sieve (p:ps) xs = h ++ sieve ps [x | x <- t, rem x p /=0]
  where (h,~(_:t)) = span(< p*p) xs