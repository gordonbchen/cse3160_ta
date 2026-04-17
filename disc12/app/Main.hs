module Main (main) where

import System.Environment
import System.IO (hFlush, stdout)


main :: IO ()
main = do
    args <- getArgs
    let seed = case args of
            [n] -> read n :: Int
            _ -> 0
    let rand = randomGen 42 101 seed 17
    let secret = (rand `mod` 10) + 1
    -- print secret
    guessNum secret 3

randomGen :: (Integral a) => a -> a -> a -> a -> a
randomGen a p = generate
  where
    generate s 0 = s
    generate s k = generate ((a * s + 1) `mod` p) (k - 1)

guessNum :: Int -> Int -> IO ()
guessNum n lives | lives <= 0 = do
    putStrLn "\nYou ran out of guesses."
    putStrLn $ "The correct answer was " ++ show n ++ ". Try again next time. :("
guessNum n lives = do
    putStrLn $ "\nLives remaining: " ++ show lives
    putStr "Guess: "
    hFlush stdout
    xstr <- getLine
    let x  = read xstr :: Int
    if x == n then putStrLn "Correct!" else do
        if x < n
            then putStrLn "Your guess is too small!"
            else putStrLn "Your guess is too big!"
        guessNum n (lives-1)
