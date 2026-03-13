data Rect = Rect Int Int

area :: Rect -> Int
area (Rect w h) = w * h

instance Eq Rect where
    r1 == r2 = area r1 == area r2

instance Ord Rect where
    r1 <= r2 = area r1 <= area r2

instance Show Rect where
    show (Rect w h) = init . concat $ replicate h (replicate w '+' ++ ['\n'])


class Addable a where
    add :: a -> a -> a

newtype Circle a = Circle a deriving (Show)

instance (Floating a) => Addable (Circle a) where
    add (Circle r1) (Circle r2) = Circle (sqrt $ r1*r1 + r2*r2)
