{- Excercise 1 -}

-- computes the list of digits of a positive integer
toDigits    :: Integer -> [Integer]
toDigits n
        | n <= 0 = []
        | otherwise = map (\c -> read [c]) $ show n

-- coputes the list of digits of  a positive integer inreverse order
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse . toDigits $ n

-- doubles the odd indexed digits in a list
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:zs) = x : 2*y : doubleEveryOther zs

-- sums all the digits in the list
sumDigits :: [Integer] -> Integer
sumDigits ns = sum ns

-- Towers of Hanoi
type Peg = String
type Move = (Peg, Peg)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi n source destination storage
      | n < 1     = []
      | n == 1    = [(source, destination)]
      | otherwise = hanoi (n - 1) source storage destination
                  ++ hanoi 1 source destination storage
                    ++ hanoi (n - 1) storage destination source

-- Towers of Hanoi ( 2 storage pegs )
hanoi' :: Int -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi' n source destination storage1 storage2
       | n < 1     = []
       | n <= 2    = hanoi n source destination storage1
       | otherwise = hanoi' (n - 2) source storage1 destination storage2
                      ++ hanoi' 2 source destination storage2 storage1
                        ++ hanoi' (n - 2) storage1 destination source storage2

-- Towers of Hanoi (N-pegs)
n_hanoi :: Int -> Peg -> Peg -> [Peg] -> [Move]
n_hanoi n source destination (s:ss)
        | n < 1     = []
        | n <= k    = hanoi n source destination s
        | otherwise = n_hanoi (n - k) source s (destination:ss)
                        ++ n_hanoi k source destination (reverse (s:ss))
                          ++ n_hanoi (n - k) s destination (source:ss)
        where k = length (s:ss)
