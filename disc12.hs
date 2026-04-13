import Control.Monad (guard)


-- Paths.
paths :: Integral a => a -> [[a]]
paths n = do
    x <- [1, 2]
    guard $ n - x >= 0
    p <- if n - x > 0 then paths (n - x) else [[]]
    return $ p ++ [n]


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

