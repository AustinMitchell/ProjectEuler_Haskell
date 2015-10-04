main = print(largestPalindrome [x*y | x <- [100..999], y <- [100..999], x <= y])

largestPalindrome :: [Integer] -> Integer
largestPalindrome [] 		= 0
largestPalindrome (x:xs)
	| isPalindrome $show x 	= max x (largestPalindrome xs) 
	| otherwise				= largestPalindrome xs

isPalindrome :: [Char] -> Bool
isPalindrome [] 	= True
isPalindrome (x:[]) = True
isPalindrome (x:xs) 
	| x == last xs 	= isPalindrome $init xs
	| otherwise		= False