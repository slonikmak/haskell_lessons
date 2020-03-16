module MyModule where

myFunc :: Int->Int->Int
myFunc a b = a + b

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib (x -1) + fib (x - 2)

fibOf :: Int->[Int]
fibOf x = [fib a | a <- take x [0,1..] ]

quiqSort :: [Int]->[Int]
quiqSort [] = []
quiqSort (x:xs) = quiqSort [y | y <- xs, y <= x] ++ [x] ++ quiqSort [y | y <- xs, y > x]

myMap :: (Int->Int)->[Int]->[Int]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

-- ФВП

zipWith' :: (a->b->c)->[a]->[b]->[c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a->b->c)->b->a->c
flip' f = g
  where g x y = f y x

largeDevisible :: Integer
largeDevisible = head (filter p [100000,99999..])
  where p x = x `mod` 3829 == 0

