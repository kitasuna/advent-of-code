module Day08 where

import Text.ParserCombinators.Parsec
import Data.List (find)
import qualified Data.Map as M

data Header = Header {
  chidCount :: Int,
  metaCount :: Int
} deriving (Eq, Show)

data Node = Node {
  header :: Header,
  chids :: [Node],
  meta :: [[ Char ]]
} deriving (Eq, Show)

data NoKidsNode = NoKidsNode {
  nkheader :: Header,
  nkmeta :: [[ Char ]]
} deriving (Eq, Show)

-- String -> [Node] (parsing)
-- [Node] -> Int (summation)

-- parseInput :: String -> Either ParseError [Node]
parseInput08 = parse pNode "goobers"

pLicenseFile :: GenParser Char st Node
pLicenseFile = do
  result <- pNode
  eof
  return result

pHeader :: GenParser Char st Header
pHeader = do
  chids <- many1 digit
  space
  metas <- many1 digit
  optional space
  return Header { chidCount = (read chids :: Int), metaCount = (read metas :: Int) }

pNode :: GenParser Char st Node
pNode = do
  h <- pHeader
  cs <- count (chidCount h) pNode
  ms <- count (metaCount h) pMetaEntry
  return Node { header=h, chids=cs, meta=ms}

pNoKidsNode :: GenParser Char st NoKidsNode
pNoKidsNode = do
  h <- pHeader
  ms <- count (metaCount h) pMetaEntry
  return NoKidsNode { nkheader=h, nkmeta=ms}

pMetaEntry :: GenParser Char st [Char]
pMetaEntry = do
  ds <- many1 digit
  optional space
  return ds

getMeta :: Node -> [Int]
getMeta n = (fmap (read :: String -> Int) (meta n)) ++ (chids n >>= getMeta)

testInput08 = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
-- testInput08 = "0 3 62 7x 8"
-- testInput08 = "0 3 6 7 8"

