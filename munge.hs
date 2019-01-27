module Munge where

import CSVParser
import Text.ParserCombinators.Parsec
import Data.List

-- | Take a parsed line, with the first element being metadata and the remainer being data
-- |   and turn that data into a nice markdown document
-- Examples:
-- >>> munge [["Website;Full Name", "jack.ellenberger.zone","","Jack Ellenberger"], [""]]
-- ["Website: jack.ellenberger.zone","Full Name: Jack Ellenberger"]
-- >>> munge [["Company;_skip;_take;", "Braintree", "Skip me", "I did some stuff"]]
-- ["Company: Braintree","I did some stuff\n"]
munge :: [[String]] -> [String]
munge parsedLines =
  concat $ map createDoc $ filterLines parsedLines

-- | Take structured, cleaned input and accumulate an md doc
-- >>> createDoc ["Key", "Value"]
-- ["Key: Value"]
-- >>> createDoc ["_take;_skip;_take;","take me", "skip me", "take me too"]
-- ["take me\n","take me too\n"]
createDoc :: [String] -> [String]
createDoc (metadata:celldata) = do
  let metaCell = quotedCell <|> many (noneOf ";\n\r")
  let metaLine = sepBy metaCell (char ';')
  let metaParser = endBy metaLine eol
  case parse metaParser "(unknown)" (metadata ++ "\n\r") of
       Left err -> return $ show err
       Right res -> do
         let meta = concat $ filterLines res
         let cell = filter keepCell celldata
         concat $ map templateMetadata $ zip meta cell

-- | Given a tuple of key, value, return "Key: Value"
-- |   or "Value" when key == "_take"
-- |   or "" when key == "_skip"
-- >>> templateMetadata ("Key", "Value")
-- ["Key: Value"]
-- >>> templateMetadata ("_take", "take this string")
-- ["take this string\n"]
-- >>> templateMetadata ("_skip", "skip this string")
-- []
templateMetadata :: (String, String) -> [String]
templateMetadata dataPair = case dataPair of ("_skip", _) -> []
                                             ("_take", val) -> [val ++ "\n"]
                                             ("_takeLn", val) -> [val ++ "\n\n"]
                                             ("_lnTake", val) -> ["\n" ++ val ++ "\n"]
                                             (key, val) -> [key ++ ": " ++ val ++ "\n"]

-- | Given a list of string lists (see: a list of lines of words)
-- |   remove lines marked for skipping and empty words
-- Examples:
-- >>> filterLines [["", "skip", "me"], ["keep", "", "me"]]
-- [["keep","me"]]
filterLines :: [[String]] -> [[String]]
filterLines lines =
  map (filter keepCell) $ filter keepLine lines

-- | False if first element is an empty string or "_skip_line", True otherwise
-- Examples:
-- >>> keepLine ["", "anything"]
-- False
-- >>> keepLine ["anything", "else"]
-- True
keepLine :: [String] -> Bool
keepLine xs = case xs of ("":_) -> False
                         [] -> False
                         xs -> True

-- |  False if empty string, True otherwise
-- Examples:
-- >>> keepCell ""
-- False
-- >>> keepCell "anything"
-- True
keepCell :: String -> Bool
keepCell str = case str of "" -> False
                           str -> True
