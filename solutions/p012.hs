import Data.Set (Set)
import qualified Data.Set as Set

main = print (Set.fromList (take 20 primeList))

primeList = take 500 primes

primeFactors :: Integer -> Integer -> [(Integer, [(Integer, Integer)])]
primeFactors 0 0 = (3, [(3, 1)]) : primeFactors 4 6
primeFactors i n = (n, factorList n primeList [] False) : (primeFactors (i+1) (n+i))
	
factorList :: Integer -> [Integer] -> [(Integer, Integer)] -> Bool -> [(Integer, Integer)]
factorList 0 _1 _2 _3 = _2
factorList n (p:ps) [] _
	| n `rem` p == 0 	= factorList (n `div` p) (p:ps) [(p, 1)] True
	| otherwise 		= factorList n ps [] False
factorList n (p:ps) (f:fs) stillFactoring
	| n `rem` p == 0 	=
		if stillFactoring then 	factorList (n `div` p) (p:ps) [(fst f, (snd f)+1):fs] True
		else 					factorList (n `div` p) (p:ps) [(p, 1):f:fs] True
	| otherwise			= factorList n ps (f:fs) False

-- Faster Eratos prime generation, drawn from http://www.justinshield.com/2011/05/haskell-a-better-prime-number-list-using-sieve-of-eratosthenes-and-lazy-pattern-matching/
primes :: [Integer]
primes = 2: 3: sieve (tail primes) [5,7..]  
 
sieve (p:ps) xs = h ++ sieve ps [x | x <- t, rem x p /=0]
  where (h,~(_:t)) = span(< p*p) xs