module MsgParser
( msgJson
) where

import Text.ParserCombinators.Parsec

msgJson :: GenParser Char st [(String,String)]
msgJson = do
  char '{'
  pairs <- sepBy keyVal pairSep
  char '}'
  return pairs

keyVal :: GenParser Char st (String,String)
keyVal = do
  optSpace
  k <- jsonKey
  kvSep
  v <- jsonVal
  return (k,v)

optSpace :: GenParser Char st String
optSpace = do
  many space

jsonKey :: GenParser Char st String
jsonKey = do
  char '"'
  key <- identifier
  char '"'
  return key

-- NOT GENERIC.  Specific to expected JSON format.
-- Does not handle String values.
jsonVal :: GenParser Char st String
jsonVal = do
  many (noneOf ",}")

identifier :: GenParser Char st String
identifier =
    many1 $ alphaNum <|> oneOf "_"

kvSep :: GenParser Char st String
kvSep = do
  optSpace
  s <- string ":"
  optSpace
  return s

pairSep :: GenParser Char st String
pairSep = do
  optSpace
  s <- string ","
  optSpace
  return s
