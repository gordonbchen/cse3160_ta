data TNode a = Leaf | Node (TNode a) (TNode a) a
    deriving Show

data Crumb a = LeftCrumb (TNode a) a | RightCrumb (TNode a) a
    deriving Show

-- type TreeZipper a = (TNode a, [Crumb a])


toZipper :: TNode a -> (TNode a, [Crumb a])
toZipper x = (x, [])

goLeft :: (TNode a, [Crumb a]) -> (TNode a, [Crumb a])
goLeft (Node l r v, crumbs) = (l, LeftCrumb r v : crumbs)

goRight :: (TNode a, [Crumb a]) -> (TNode a, [Crumb a])
goRight (Node l r v, crumbs) = (r, RightCrumb l v : crumbs)

goUp :: (TNode a, [Crumb a]) -> (TNode a, [Crumb a])
goUp (l, LeftCrumb r v : crumbs) = (Node l r v, crumbs)
goUp (r, RightCrumb l v : crumbs) = (Node l r v, crumbs)
