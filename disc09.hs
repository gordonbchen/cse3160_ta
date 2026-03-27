-- Traffic.
data TrafficLight = Red | Yellow | Green deriving (Eq, Show)

nextLight :: TrafficLight -> TrafficLight
nextLight Red = Green
nextLight Green = Yellow
nextLight Yellow = Red


-- Tree.
data TNode a = Leaf | Node (TNode a) (TNode a) a deriving (Show)

tfold :: (b -> b -> a -> b) -> b -> TNode a -> b
tfold _ acc Leaf = acc
tfold f acc (Node l r v) = f lacc racc v
    where lacc = tfold f acc l
          racc = tfold f acc r


tAll :: (a -> Bool) -> TNode a -> Bool
tAll f = tfold (\la ra v -> la && ra && f v) True

tMax :: (Ord a, Bounded a) => TNode a -> a
tMax = tfold (\la ra v -> maximum [la, ra, v]) minBound

tMax' :: (Ord a) => TNode a -> Maybe a
tMax' Leaf = Nothing
tMax' t@(Node l r v0) = Just $ tfold (\la ra v -> maximum [la, ra, v]) v0 t


-- Zippers.
type Zipper a = ([a], [a])

toZipper :: [a] -> Zipper a
toZipper x = ([], x)

isBegin :: Zipper a -> Bool
isBegin ([], _) = True
isBegin _ = False

insertLeft :: Zipper a -> a -> Zipper a
insertLeft (l, r) x = (l, x:r)

replaceFocus :: Zipper a -> a -> Maybe (Zipper a)
replaceFocus (_, []) _ = Nothing
replaceFocus (l, h : t) x = Just (l, x : t)


-- Monoids.
data Logged a = Logged {getA :: a, log :: [String]} deriving (Show)

instance (Semigroup a) => Semigroup (Logged a) where
    Logged x l1 <> Logged y l2 = Logged (x <> y) (l1 ++ l2)

instance (Monoid a) => Monoid (Logged a) where
    mempty = Logged mempty []


newtype Product a = Product {getProduct :: a} deriving (Show)

instance (Num a) => Semigroup (Product a) where
    Product x <> Product y = Product (x * y)

instance (Num a) => Monoid (Product a) where
    mempty = Product 1


logMult :: (Num a, Show a) => [a] -> Logged (Product a)
logMult = foldMap (\x -> Logged (Product x) ["Multiplied by: " ++ show x])
