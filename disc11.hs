import Control.Monad (guard)

-- Pythagorean Theorem.
pythag :: Integral a => a -> [(a, a, a)]
pythag n = do
    a <- [1..n]
    b <- [1..n]
    c <- [1..n]
    guard (a <= b && b <= c && a*a + b*b == c*c)
    return (a, b, c)

pythag' :: Integral a => a -> [(a, a, a)]
pythag' n = [1..n] >>= \a ->
           [1..n] >>= \b ->
           [1..n] >>= \c -> 
           guard (a <= b && b <= c && a*a + b*b == c*c) >>
           return (a, b, c)


-- JSON.
db = [("users", [("1", "Ethan"), ("2", "Heri"), ("3", "Gordon")]),
      ("metadata", [("Ethan", "Admin"), ("Heri", "Noob"), ("Gordon", "Pleb")])]

getAccess :: Int -> [(String, [(String, String)])] -> Maybe String
getAccess userid db = do
    users <- lookup "users" db
    name <- lookup (show userid) users
    roles <- lookup "metadata" db
    lookup name roles


-- Cartesian product.
makePair :: [a] -> [b] -> [(a, b)]
makePair xs ys = [(x, y) | x <- xs, y <- ys]

makePair' :: [a] -> [b] -> [(a, b)]
makePair' xs ys = do
    x <- xs
    y <- ys
    return (x, y)

makePair'' :: [a] -> [b] -> [(a, b)]
makePair'' xs ys = xs >>= (\x -> ys >>= (\y -> return (x, y)))


-- Paths.
paths :: Integral a => a -> [[a]]
paths n = do
    x <- [1, 2]
    guard $ n - x >= 0
    p <- if n - x > 0 then paths (n - x) else [[]]
    return $ p ++ [n]
