main = print (largestCollatz numList)

numList = [(x,x) | x <- [1..1000000]]

largestCollatz :: [(Integer, Integer)] -> Integer
largestCollatz [] 			= 0
largestCollatz ((x,n):[]) 	= x
largestCollatz (x:xs)		= largestCollatz (iterateCollatz (x:xs))

iterateCollatz :: [(Integer, Integer)] -> [(Integer, Integer)]
iterateCollatz [] 			= []
iterateCollatz ((x, n):xs)	
	| n == 1				= iterateCollatz xs
	| n `rem` 2 == 0		= (x, n`div`2) : iterateCollatz xs
	| otherwise				= (x, 3*n + 1) : iterateCollatz xs