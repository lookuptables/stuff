
-- Find index of character inside String, if any
    
pos :: Char -> [Char] -> Int
pos ch [] = -1
pos ch st  
    | null lop = -1
    | otherwise = snd $ head lop
    where lop = [x | x <- zip st [0..], fst x == ch]

-- primes
primes = filterPrime [2..]
    where filterPrime (p:xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]

-- tests
test1 []  = []
test1 (x:xs) = test1 xs ++ [x]

test2 x = res
    where res = y
          y = 3 + z
          z = 7 + x

test3 [] = []
test3 (x:xs) 
    | x == 3 = 1 : test3 xs
    | otherwise = 0 : test3 xs 
test3 (_:xs) = [1]

-- import System.IO.Unsafe
-- let text = unsafePerformIO . readFile $ "file.txt"

