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

tsize2 :: Node a -> Int
tsize2 = tfold (\lacc racc x -> lacc + racc + 1) 0

tsort2 :: Node a -> [a]
tsort2 = tfold (\lacc racc x -> lacc ++ [x] ++ racc) []

theight1 :: Node a -> Int
theight1 = tfold (\lacc racc x -> 1 + max lacc racc) 0


data Crumb a = Lc a (Node a) | Rc a (Node a)
    deriving (Show)

goLeft :: (Node a, [Crumb a]) -> (Node a, [Crumb a])
goLeft (Node l r v, cs) = (l, Lc v r : cs)

goRight :: (Node a, [Crumb a]) -> (Node a, [Crumb a])
goRight (Node l r v, cs) = (r, Rc v l : cs)

goUp :: (Node a, [Crumb a]) -> (Node a, [Crumb a])
goUp (t, (Lc v r) : cs) = (Node t r v, cs)
goUp (t, (Rc v l) : cs) = (Node l t v, cs)

goRoot :: (Node a, [Crumb a]) -> (Node a, [Crumb a])
goRoot (t, []) = (t, [])
goRoot cs = goRoot (goUp cs)

replaceValue :: a -> (Node a, [Crumb a]) -> (Node a, [Crumb a])
replaceValue x (Node l r _, cs) = (Node l r x, cs)


hinsert :: Ord a => a -> Node a -> Node a
hinsert x Leaf = Node Leaf Leaf x
hinsert x (Node l r v) = Node r (hinsert maxv l) minv
    where minv = min x v
          maxv = max x v

hextract :: Ord a => Node a -> (a, Node a)
hextract (Node l r v) = (v, combine l r)
    where combine Leaf r = r
          combine l Leaf = l
          combine (Node ll lr lv) r@(Node rl rr rv) | lv <= rv = Node (combine ll lr) r lv
          combine l@(Node ll lr lv) (Node rl rr rv) = Node l (combine rl rr) rv
