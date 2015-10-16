main = print(nCr 40 20)

fac :: Integer -> Integer
fac 0 = 1
fac n = product [1..n] 

nCr :: Integer -> Integer -> Integer
nCr n r = (fac n) `div` (fac (n-r) * fac r)