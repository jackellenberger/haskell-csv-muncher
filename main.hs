module Main where

import CSVParser
import Munge
import Text.ParserCombinators.Parsec

main =
    do c <- getContents
       case parse csvFile "(stdin)" (c ++ "\n") of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> do putStrLn $ concat $ munge r
