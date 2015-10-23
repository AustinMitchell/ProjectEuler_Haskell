import qualified Data.Vector as V

main = print (numArrangements numTileSlots tileSet)
--main = print (numTiles 10 tileSet)


numTileSlots = 5
facList = V.fromList(take (numTileSlots+1) (buildFactorial 0 0))
tileSet = [(2,0),(3,0),(4,0)]

-- Builds a list of factorials in O(n) time
--		n: Current index to compute. Should start at 0 to run properly
--		x: Last number computed. Input is unimportant if i is 0
buildFactorial :: Integer -> Integer -> [Integer]
buildFactorial 0 _ = [1, 1] ++ buildFactorial 2 1 
buildFactorial n x = (n*x) : buildFactorial (n+1) (n*x)

fac :: Int -> Integer
fac n = facList V.! n

-- Takes a list of tile to number of tile pairs and gives the number of combinations this divides a total number of cominations by
--		(t:tn):	A list of tile-width to number of tile pairs
--			t: 	Tile width
--			tn:	Number of tiles
arrangementsFromTiles :: [(Int, Int)] -> Integer
arrangementsFromTiles [] 			= 1
arrangementsFromTiles ((t,tn):ts) 	= (fac tn) * (arrangementsFromTiles ts) 

-- Takes in the same list and the total number of tiles and returns the amount of empty tiles
numEmptyTiles :: Int -> [(Int, Int)] -> Int
numEmptyTiles n [] 			= n
numEmptyTiles n ((t,tn):ts)	= numEmptyTiles (n-tn*t) ts

-- Takes in the same thing but returns the number of tiles (total empty tiles plus number of coloured tiles)
numTiles :: Int -> [(Int, Int)] -> Int
numTiles n [] 			= n
numTiles n ((t,tn):ts)	= numTiles (n-tn*(t-1)) ts

-- Generates the next tileset given a tileset ((t,tn):ts). Returning [] indicates that you have no more tilesets to run through.
--		n:			Current number of empty tiles
--		result:		For functional purposes; Pass in []
--		((t,tn):ts)	The original tileset
generateNextTilePairs :: Int -> [(Int,Int)] -> [(Int, Int)] -> [(Int, Int)]
generateNextTilePairs n result ((t,tn):[])
	| t > n 	= []
	| otherwise	= result ++ [(t,tn+1)]
generateNextTilePairs n result ((t,tn):ts) 
	| t > n 	= generateNextTilePairs (n+tn*(t-1)) (result++[(t,0)]) ts
	| otherwise	= result ++ [(t,tn+1)] ++ ts

-- Returns the number of arrangements for your given tile count and tileset
--		(t:ts):	Original tileset. Pass in [(a,0),(b,0),(c,0)...] where a>0, a<=b, b<=c, and so on
--		m:		Number of tile slots to work with
numArrangements :: Int -> [(Int, Int)] -> Integer
numArrangements	_ []		= 0 
numArrangements	m tileSet 	= ((fac (numTiles m tileSet)) `div` ((fac emptyTiles) * (arrangementsFromTiles tileSet))) +
								numArrangements m (generateNextTilePairs emptyTiles [] tileSet)
	where 
		emptyTiles = numEmptyTiles m tileSet