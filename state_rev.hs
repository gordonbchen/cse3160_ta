import Control.Monad.State
import Control.Monad


push :: a -> [a] -> ((), [a])
push x xs = ((), x:xs)

pop :: [a] -> (a, [a])
pop (h:t) = (h, t)

popSnd :: [a] -> (a, [a])
popSnd xs = (x2, s3)
    where (x1, s1) = pop xs
          (x2, s2) = pop s1
          (_, s3) = push x1 s2

addTop3 :: (Num a) => [a] -> ((), [a])
addTop3 xs = push (x1 + x2 + x3) s3
    where (x1, s1) = pop xs
          (x2, s2) = pop s1
          (x3, s3) = pop s2


pushS :: a -> State [a] ()
pushS x = state $ \xs -> ((), x:xs)

popS :: State [a] a
popS = state $ \(h:t) -> (h, t)

popSndS :: State [a] a
popSndS = do
    x1 <- popS
    x2 <- popS
    pushS x1
    return x2

addTop3S :: (Num a) => State [a] ()
addTop3S = do
    x1 <- popS
    x2 <- popS
    x3 <- popS
    pushS $ x1 + x2 + x3

onlyTop :: State [a] ()
onlyTop = do
    x1 <- popS
    put [x1]

prod :: Int -> State Int ()
prod n = forM_ [1..n] (\x -> modify (* x))

forM' :: (Monad m) => [a] -> (a -> m b) -> m [b]
forM' [] _ = return []
forM' (h:t) f = do
    res <- f h
    ress <- forM' t f
    return $ res : ress

forM_' :: (Monad m) => [a] -> (a -> m b) -> m ()
forM_' [] _ = return ()
forM_' (h:t) f = do
    f h
    forM_' t f

fib :: State (Int, Int) Int
fib = state $ \(x1, x2) -> (x1, (x2, x1 + x2))

fibs :: Int -> [Int]
fibs n = evalState (forM [1..n] (\_ -> fib)) (0, 1)

fibs' :: Int -> [Int]
fibs' n = evalState (replicateM n fib) (0, 1)

sumM :: [Int] -> Int
sumM xs = execState (forM_ xs (\x -> modify (x +))) 0
