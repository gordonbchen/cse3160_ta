import GHC.Base (sconcat)
import qualified Data.List.NonEmpty as NE


-- Max semigroup.
newtype Max a = Max {getMax :: a} deriving (Show)

instance (Ord a) => Semigroup (Max a) where
    (Max x) <> (Max y) = Max (max x y)

sconcat' :: (Semigroup a) => [a] -> a
sconcat' (h : t) = GHC.Base.sconcat $ h NE.:| t

smax :: (Bounded a, Ord a) => [a] -> a
smax [] = minBound
smax xs = getMax . sconcat' . map Max $ xs


-- Rewrite smax by making Max a monoid.
instance (Ord a, Bounded a) => Monoid (Max a) where
    mempty = Max minBound

mmax :: (Bounded a, Ord a) => [a] -> a
mmax = getMax . mconcat . map Max


-- Mean monoid.
data Mean a = Mean a Int deriving (Show)

calcMean :: (Fractional a) => Mean a -> Maybe a
calcMean (Mean _ 0) = Nothing
calcMean (Mean s n) = Just (s / fromIntegral n)

instance (Fractional a) => Semigroup (Mean a) where
    (Mean s1 n1) <> (Mean s2 n2) = Mean (s1 + s2) (n1 + n2)

instance (Fractional a) => Monoid (Mean a) where
    mempty = Mean 0 0

mmean :: (Fractional a) => [a] -> Maybe a
mmean = calcMean . mconcat . map (\x -> Mean x 1)


-- Rewrite mmax and mmean using foldMap.
fmax :: (Bounded a, Ord a) => [a] -> a
fmax = getMax . foldMap Max

fmean :: (Floating a) => [a] -> Maybe a
fmean = calcMean . foldMap (\x -> Mean x 1)
