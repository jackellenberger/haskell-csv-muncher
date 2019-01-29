module CSVParser where

import Text.ParserCombinators.Parsec
import Text.StringLike

-- Taken in part from Real World Haskell
-- http://book.realworldhaskell.org/read/using-parsec.html

separatedValues :: Char -> GenParser Char st [[String]]
separatedValues sepChar = do
  let cell = quotedCell <|> many (noneOf (sepChar:"\n\r"))
  let line = sepBy cell (char sepChar)
  endBy line eol

quotedCell =
    do char '"'
       content <- many quotedChar
       char '"' <?> "quote at end of cell"
       return content

quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> try (string "\n")
    <|> try (string "\r")
    <?> "end of line"
