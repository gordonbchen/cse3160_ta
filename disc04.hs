-- Custom data types.
type Dict k v = [(k, v)]

dfind :: (Eq k) => k -> Dict k v -> Maybe v
dfind _ [] = Nothing
dfind x ((k, v) : t) | x == k = Just v
                     | otherwise = dfind x t

dfind1 :: (Eq k) => k -> Dict k v -> Maybe v
dfind1 x d = case filter ((==) x . fst) d of
    [(k, v)] -> Just v
    [] -> Nothing

dinsert :: (Eq k) => k -> v -> Dict k v -> Dict k v
dinsert k v [] = [(k, v)]
dinsert k v ((x, y) : t) | k == x = (x, v) : t
                         | otherwise = (x, y) : dinsert k v t

make :: (Eq a, Enum a, Num a) => a -> Dict a a
make n = foldr (\x -> dinsert x (x*100)) [] [1..n]


-- Sorting.
selectSort :: (Ord a) => [a] -> [a]
selectSort [] = []
selectSort xs = s : selectSort rem
    where (s, rem) = extractMin xs

extractMin :: (Ord a) => [a] -> (a, [a])
extractMin [a] = (a, [])
extractMin (h : t) | h < s = (h, s : rem)
                   | otherwise = (s, h : rem)
    where (s, rem) = extractMin t

insertSort :: (Ord a) => [a] -> [a]
insertSort = foldr insert []

insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (h : t) | x <= h = x : h : t
                 | otherwise = h : insert x t

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (h : t) = quickSort ls ++ [h] ++ quickSort rs
    where ls = filter (<= h) t
          rs = filter (> h) t


-- BST.
data Node a = Leaf | Node (Node a) (Node a) a
    deriving Show

tmember :: (Ord a) => a -> Node a -> Bool
tmember _ Leaf = False
tmember x (Node l r v) | x == v = True
                       | x < v = tmember x l
                       | otherwise = tmember x r

tinsert :: (Ord a) => a -> Node a -> Node a
tinsert x Leaf = Node Leaf Leaf x
tinsert x t@(Node l r v) | x == v = t
                         | x < v = Node (tinsert x l) r v
                         | otherwise = Node l (tinsert x r) v

tfromList :: (Ord a) => [a] -> Node a
tfromList = foldr tinsert Leaf

myTree = tfromList [1, 5, 2, 3, 4]

ttoList :: Node a -> [a]
ttoList Leaf = []
ttoList (Node l r v) = v : ttoList l ++ ttoList r

tsize :: Node a -> Int
tsize Leaf = 0
tsize (Node l r v) = 1 + tsize l + tsize r

theight :: Node a -> Int
theight Leaf = 0
theight (Node l r v) = 1 + max (theight l) (theight r)

tfoldi :: (b -> a -> b) -> b -> Node a -> b
tfoldi f acc Leaf = acc
tfoldi f acc (Node l r v) = tfoldi f newAcc r
    where leftAcc = tfoldi f acc l
          newAcc = f leftAcc v

inOrder :: Node a -> [a]
inOrder = tfoldi (\acc x -> acc ++ [x]) []

postOrder :: Node a -> [a]
postOrder = tfoldi (flip (:)) []

tsize1 :: Node a -> Int
tsize1 = tfoldi (\acc x -> acc + 1) 0

tfold :: (b -> b -> a -> b) -> b -> Node a -> b
tfold f acc Leaf = acc
tfold f acc (Node l r v) = f lacc racc v
    where lacc = tfold f acc l
          racc = tfold f acc r

theight1 :: Node a -> Int
theight1 = tfold (\lacc racc v -> 1 + max lacc racc) 0
