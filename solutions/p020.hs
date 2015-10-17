import Data.Char

main = print(sum [digitToInt x | x <- (show (product[1..100]))])