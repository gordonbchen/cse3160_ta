-- Shape Functor.
data Shape a = Circle a | Rectangle a a deriving (Show)

instance Functor Shape where
    fmap f (Circle r) = Circle (f r)
    fmap f (Rectangle l w) = Rectangle (f l) (f w)


-- Thermometer.
parseTemp :: String -> Either String Double 
parseTemp "freezing" = Right 0.0 
parseTemp "boiling"  = Right 100.0 
parseTemp "body"     = Right 37.0 
parseTemp x          = Left $ "Unknown temperature: " ++ x 

tempDiff :: String -> String -> Either String Double
tempDiff x y = (<$>) abs $ (-) <$> parseTemp x <*> parseTemp y


-- Triplet.
data Triplet a = Triplet a a a deriving (Show, Eq)

instance Functor Triplet where
    fmap f (Triplet x y z) = Triplet (f x) (f y) (f z)

instance Applicative Triplet where
    pure x = Triplet x x x
    (Triplet a b c) <*> (Triplet x y z) = Triplet (a x) (b y) (c z)


-- Combs.
combs :: [[a]] -> [[a]]
combs = foldr (\x acc -> (:) <$> x <*> acc) [[]]

bitstrings :: Int -> [[Int]]
bitstrings n = combs $ replicate n [0, 1]
