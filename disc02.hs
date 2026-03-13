import Data.Char (isPunctuation, isAlpha)

everyOther1 :: [a] -> [a]
everyOther1 [] = []
everyOther1 [x] = [x]
everyOther1 (h : _ : t) = h : everyOther1 t

everyOther2 :: [a] -> [a]
everyOther2 xs = map fst $ filter (even . snd) $ zip xs [0..]

everyOther3 :: [a] -> [a]
everyOther3 xs = reverse . snd $ foldl (\(i, eo) x -> if even i then (i+1, x : eo) else (i+1, eo)) (0, []) xs

everyOther4 :: [a] -> [a]
everyOther4 xs = [x | (x, i) <- zip xs [0..], even i]


nPunct1 :: String -> Int
nPunct1 [] = 0
nPunct1 (h : t) = (if isPunctuation h then 1 else 0) + nPunct1 t

nPunct2 :: String -> Int
nPunct2 = length . filter isPunctuation

nPunct3 :: String -> Int
nPunct3 = foldr (\x acc -> if isPunctuation x then acc + 1 else acc) 0


longestWordLen1 :: String -> Int
longestWordLen1 = maximum . map (length . filter isAlpha) . words

longestWordLen2 :: String -> Int
longestWordLen2 str = max l1 l2
    where (l1, l2) = foldl aux (0, 0) str
          aux (maxl, currl) c | isAlpha c = (maxl, currl + 1)
          aux (maxl, currl) c = (max maxl currl, 0)

longestWordLen3 :: String -> Int
longestWordLen3 = aux 0 0
    where aux maxlen currlen [] = max maxlen currlen
          aux maxlen currlen (h : t) | isAlpha h = aux maxlen (currlen+1) t
                                     | otherwise = aux (max maxlen currlen) 0 t


countVIPs :: [String] -> [String] -> Int
countVIPs vips = length . filter (`elem` vips)


toPalindrone1 :: String -> String
toPalindrone1 x = foldl (flip (:)) x x

toPalindrone2 :: String -> String
toPalindrone2 x = foldr (\x acc -> acc ++ [x]) x x
