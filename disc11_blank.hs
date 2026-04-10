import Control.Monad (guard)

-- Pythagorean Theorem.
pythag :: Integral a => a -> [(a, a, a)]
pythag n = undefined

pythag' :: Integral a => a -> [(a, a, a)]
pythag' n = undefined


-- JSON.
db = [("users", [("1", "Ethan"), ("2", "Heri"), ("3", "Gordon")]),
      ("metadata", [("Ethan", "Admin"), ("Heri", "Noob"), ("Gordon", "Pleb")])]

getAccess :: Int -> [(String, [(String, String)])] -> Maybe String
getAccess userid db = undefined


-- Cartesian product.
makePair :: [a] -> [b] -> [(a, b)]
makePair xs ys = undefined

makePair' :: [a] -> [b] -> [(a, b)]
makePair' xs ys = undefined

makePair'' :: [a] -> [b] -> [(a, b)]
makePair'' xs ys = undefined


-- Paths.
paths :: Integral a => a -> [[a]]
paths n = undefined
