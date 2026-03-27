data Shape a = Circle a
    | Rect a a
    | Triangle a a a
    deriving (Show, Eq)

area :: (Floating a) => Shape a -> a
area (Circle r) = pi * r*r
area (Rect w h) = w * h
area (Triangle a b c) = sqrt $ s * (s-a) * (s-b) * (s-c)
    where s = (a + b + c) / 2


newtype Product a = Product {getProduct :: a} deriving (Show)

instance (Num a) => Semigroup (Product a) where
    (Product x) <> (Product y) = Product (x * y)

instance (Num a) => Monoid (Product a) where
    mempty = Product 1

mproduct :: (Num a) => [a] -> a
mproduct = getProduct . mconcat . map Product

fproduct :: (Num a) => [a] -> a
fproduct = getProduct . foldMap Product

safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (h:t) 0 = Just h
safeIndex (h:t) n = safeIndex t (n-1)
