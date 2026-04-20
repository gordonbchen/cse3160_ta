import System.Environment

main = do
    args <- getArgs
    case args of
        [src, dst] -> do
            content <- readFile src
            writeFile dst content
        _ -> putStrLn "Incorrect number of arguments. Usage: ./copy <source> <dest>"

