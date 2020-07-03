import Data.Char

start = ord 'A'
end = ord 'Z'
difference = end - start + 1

pair :: [a] -> [b] -> [(a, b)]
pair ptx key = zip ptx $ cycle key

shift :: (Char, Char) -> (Int -> Int -> Int) -> Char
shift (a, b) fn = chr $ start + (ord a `fn` ord b) `mod` difference

cipher' :: [Char] -> [Char] -> (Int -> Int -> Int) -> [Char]
cipher' ptx key fn = map (flip shift fn) $ pair ptx key

encode, decode :: [Char] -> [Char] -> [Char]
encode ptx key = cipher' ptx key (+)
decode ptx key = cipher' ptx key (-)

plainText = "VIGENERECIPHER"
secret = "LOVE"
