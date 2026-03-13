takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' cond (h : t) | cond h = h : takeWhile' cond t
                        | otherwise = []

mapr :: (a -> b) -> [a] -> [b]
mapr f = foldr (\x acc -> f x : acc) []

maplBad :: (a -> b) -> [a] -> [b]
maplBad f = foldl (\acc x -> acc ++ [f x]) []

mapl :: (a -> b) -> [a] -> [b]
mapl f = reverse . foldl (\acc x -> f x : acc) []

filterr :: (a -> Bool) -> [a] -> [a]
filterr f = foldr (\x acc -> if f x then x : acc else acc) []

filterlBad :: (a -> Bool) -> [a] -> [a]
filterlBad f = foldl (\acc x -> if f x then acc ++ [x] else acc) []

filterl :: (a -> Bool) -> [a] -> [a]
filterl f = reverse . foldl (\acc x -> if f x then x : acc else acc) []

count _ [] = 0
count x (h : t) | x == h = 1 + count x t
                | otherwise = count x t

foldl' _ acc [] = acc
foldl' f acc (h : t) = foldl' f (f acc h) t
