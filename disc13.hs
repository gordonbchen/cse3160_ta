{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, encodeFile)
import qualified Data.ByteString.Lazy.Char8 as BS (pack)
import System.Random (StdGen, uniformR)


mapM' :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (h:t) = do
    x <- f h
    rem <- mapM' f t
    return $ x : rem

mapM_' :: (Monad m) => (a -> m b) -> [a] -> m ()
mapM_' f [] = return ()
mapM_' f (h:t) = do
    f h
    mapM_' f t

mapM_'' :: (Monad m, Foldable t) => (a -> m b) -> t a -> m ()
mapM_'' f = foldr (\x acc -> f x >> acc) (return ())


absoluteLog :: [Int] -> IO [Int]
absoluteLog = mapM logAndAbs
  where
    logAndAbs x = do
        putStrLn $ "Computing absolute value of " ++ show x ++ "..."
        return (abs x)


countdown :: [Int] -> IO ()
countdown xs = do
    mapM_ (\x -> putStrLn $ show x ++ "!") xs
    putStrLn "Blast Off!"


-- JSON.
data Book = Book 
    { title  :: String
    , author :: String
    , year   :: Int 
    } deriving (Show, Read, Eq, Generic)

instance FromJSON Book
instance ToJSON Book

documentBooks :: FilePath -> [Book] -> IO ()
documentBooks fname books = writeFile fname $ BS.unpack (encode books)


-- Random.
getRandChar :: StdGen -> (Char, StdGen)
getRandChar = uniformR ('a', 'z')

generateRandString :: Int -> StdGen -> (String, StdGen)
generateRandString 0 gen = ("", gen)
generateRandString n gen = 
    let (c, nextGen) = getRandChar gen
        (rest, finalGen) = generateRandString (n - 1) nextGen
    in (c : rest, finalGen)

isWord :: String -> [String] -> Bool
isWord word dict = word `elem` dict

checkRandomWord :: Int -> StdGen -> IO ()
checkRandomWord len gen = do
    let (word, _) = generateRandString len gen
    content <- readFile "words.txt"
    let dict = lines content
    if isWord word dict
        then putStrLn $ word ++ " is a real word!"
        else putStrLn $ word ++ " is gobbledygook."
