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


-- MyEither.
data MyEither a b = MyLeft a | MyRight b deriving (Show)

instance Functor (MyEither a) where
    fmap f (MyLeft x) = MyLeft x
    fmap f (MyRight x) = MyRight (f x)

instance Applicative (MyEither a) where
    pure = MyRight
    (MyRight x) <*> (MyRight y) = MyRight (x y)
    (MyLeft e) <*> (MyRight _) = MyLeft e
    (MyRight _) <*> (MyLeft e) = MyLeft e
    (MyLeft e) <*> (MyLeft _) = MyLeft e

instance Monad (MyEither a) where
    (MyLeft e) >>= f = MyLeft e
    (MyRight x) >>= f = f x

safeDec :: Int -> MyEither String Int
safeDec x | x - 1 < 0 = MyLeft "Could not decrement."
          | otherwise = MyRight $ x - 1

