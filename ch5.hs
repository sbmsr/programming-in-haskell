import Data.Char

{- 1. Using a list comprehension, give an expression
that calculates the sum 1^2 + 2^2 + ... 100^2 of the
first one hundred integer squares.” -}

sumOfFirstHundredSquares :: Int
sumOfFirstHundredSquares = sum [x ^ 2 | x <- [1 .. 100]]

{-
“2.Suppose that a coordinate grid of size m × n is
given by the list of all pairs (x, y) of integers
such that 0 <= x <= m and 0 <= y <= n.

Using a list comprehension, define a function
grid :: Int -> Int -> [(Int,Int)] that returns a
coordinate grid of a given size. For example:

> grid 1 2
[(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]”
-}

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0 .. m], y <- [0 .. n]]

{-
“3.Using a list comprehension and the function grid
above, define a function square :: Int -> [(Int,Int)]
that returns a coordinate square of size n, excluding
the diagonal from (0, 0) to (n, n). For example:

> square 2
[(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]”
-}

removeDiagonals :: [(Int, Int)] -> [(Int, Int)]
removeDiagonals [] = []
removeDiagonals (x : xs)
  | uncurry (==) x = removeDiagonals xs
  | otherwise = x : removeDiagonals xs

square :: Int -> [(Int, Int)]
square n = [(x, y) | let all = grid n n, (x, y) <- removeDiagonals all]

{-
“4.In a similar way to the function length, show how
the library function replicate :: Int -> a -> [a]
that produces a list of identical elements can be
defined using a list comprehension. ”
-}
replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1 .. n]]

{-
“5.A triple (x, y, z) of positive integers is Pythagorean
if it satisfies the equation x2 + y2 = z2. Using a list
comprehension with three generators, define a function
pyths :: Int -> [(Int,Int,Int)] that returns the list of
all such triples whose components are at most a given
limit. For example:

> pyths 10
[(3,4,5),(4,3,5),(6,8,10),(8,6,10)]”
-}

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x ^ 2 + y ^ 2 == z ^ 2]

{-
“6.A positive integer is perfect if it equals the sum of
all of its factors, excluding the number itself. Using a
list comprehension and the function factors, define a
function perfects :: Int -> [Int] that returns the list
of all perfect numbers up to a given limit. For example:

> perfects 500
[6,28,496]”
-}

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n - 1], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], sum (factors x) == x]

{-
7.Show how the list comprehension
[(x,y) | x <- [1,2], y <- [3,4]] with two generators
can be re-expressed using two comprehensions with single
 generators. Hint: nest one comprehension within the other
 and make use of the library function concat :: [[a]] -> [a].
-}

nestedComprehension :: [(Int, Int)]
nestedComprehension = concat [[(x, y) | y <- [3, 4]] | x <- [1, 2]]

{-
8.Redefine the function positions using the function find.
-}

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

{-
9.The scalar product of two lists of integers 
xs and ys of length n is given by the sum of the 
products of corresponding integers:

sum from i = 0 -> n-1, of (x_i * y_i)

In a similar manner to chisqr, show how a list 
comprehension can be used to define a function 
scalarproduct :: [Int] -> [Int] -> Int 
that returns the scalar product of two lists. 

For example:

> scalarproduct [1,2,3] [4,5,6]
32
-}

scalarproduct :: [Int] -> [Int] -> Int 
scalarproduct xs ys = sum [x * y | (x,y) <- zip xs ys]


{-
10.Modify the Caesar cipher program to also handle upper-case letters.”
-}

let2int :: Char -> Int
let2int c 
  | isLower c = ord c - ord 'a'
  | otherwise = ord c - ord 'A'

int2let :: Int -> Bool -> Char
int2let n isLower
  | isLower = chr(ord 'a' + n)
  | otherwise = chr(ord 'A' + n)

shift :: Int -> Char -> Char
shift n c 
  | isAlpha c = int2let ((let2int c + n) `mod` 26) (isLower c)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
