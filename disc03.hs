curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (x, y) = f x y

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f acc [] = acc
foldl' f acc (h : t) = foldl' f (f acc h) t

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f acc [] = acc
foldr' f acc (h : t) = f h $ foldr' f acc t
