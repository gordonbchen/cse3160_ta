import Control.Monad (guard)
import Data.Char (toUpper)


-- Pythagorean Theorem.
pythag :: Integral a => a -> [(a, a, a)]
pythag n = do
    a <- [1..n]
    b <- [a..n]
    c <- [b..n]
    guard (a*a + b*b == c*c)
    return (a, b, c)

pythag' :: Integral a => a -> [(a, a, a)]
pythag' n = [1..n] >>= \a ->
           [a..n] >>= \b ->
           [b..n] >>= \c ->
           guard (a*a + b*b == c*c) >>
           return (a, b, c)


-- Paths.
paths :: Integral a => a -> [[a]]
paths 0 = [[]]
paths n = do
    x <- [n-1, n-2]
    guard $ x >= 0
    p <- paths x
    return $ p ++ [n]


-- Tiny IO.
upperLine :: IO String
upperLine = map toUpper <$> getLine

catLines :: IO String
catLines = do
    l1 <- getLine
    l2 <- getLine
    return $ l1 ++ l2

catLines' :: IO String
catLines' = (++) <$> getLine <*> getLine

