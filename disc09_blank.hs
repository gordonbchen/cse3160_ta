-- Traffic.
data TrafficLight = Todo

nextLight :: TrafficLight -> TrafficLight
nextLight = undefined


-- Tree.
data TNode a = Leaf | Node (TNode a) (TNode a) a deriving (Show)

tfold :: (b -> b -> a -> b) -> b -> TNode a -> b
tfold _ acc Leaf = acc
tfold f acc (Node l r v) = f lacc racc v
    where lacc = tfold f acc l
          racc = tfold f acc r


tAll :: (a -> Bool) -> TNode a -> Bool
tAll = undefined

tMax :: (Ord a, Bounded a) => TNode a -> a
tMax = undefined

tMax' :: (Ord a) => TNode a -> Maybe a
tMax' = undefined


-- Zippers.
type Zipper a = ([a], [a])

toZipper :: [a] -> Zipper a
toZipper x = ([], x)


isBegin :: Zipper a -> Bool
isBegin = undefined

insertLeft :: Zipper a -> a -> Zipper a
insertLeft = undefined

replaceFocus :: Zipper a -> a -> Maybe (Zipper a)
replaceFocus = undefined


-- Monoids.
newtype Product a = Product {getProduct :: a} deriving (Show)

instance (Num a) => Semigroup (Product a) where
    Product x <> Product y = Product (x * y)

instance (Num a) => Monoid (Product a) where
    mempty = Product 1


data Logged a = Todo1

logMult :: (Num a, Show a) => [a] -> Logged (Product a)
logMult = undefined
