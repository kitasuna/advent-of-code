module Main where

import qualified Data.Map                      as M
import           Text.ParserCombinators.Parsec
import           Text.Read                      ( readMaybe )

main :: IO ()
main = do
  text <- readFile "Day04.input"
  print ">>> DAY 04 <<<"
  print $ "Part 1: " ++ show (parse (file foldDatMap) "" text)
  print $ "Part 2: " ++ show (parse (file foldDatMap') "" text)

foldDatMap :: M.Map String String -> Maybe String
foldDatMap m =
  M.lookup "byr" m
    >> M.lookup "iyr" m
    >> M.lookup "eyr" m
    >> M.lookup "hgt" m
    >> M.lookup "hcl" m
    >> M.lookup "ecl" m
    >> M.lookup "pid" m

foldDatMap' :: M.Map String String -> Maybe String
foldDatMap' m =
  (M.lookup "byr" m >>= validBirthYear)
    >> (M.lookup "iyr" m >>= validIssueYear)
    >> (M.lookup "eyr" m >>= validExpirationYear)
    >> (M.lookup "hgt" m >>= validHeight)
    >> (M.lookup "hcl" m >>= validHairColor)
    >> (M.lookup "ecl" m >>= validEyeColor)
    >> (M.lookup "pid" m >>= validPassportId)

validBirthYear :: String -> Maybe String
validBirthYear s = do
  i <- readMaybe s :: Maybe Integer
  if i >= 1920 && i <= 2002 then (Just s) else Nothing

validIssueYear :: String -> Maybe String
validIssueYear s = do
  i <- readMaybe s :: Maybe Integer
  if i >= 2010 && i <= 2020 then Just s else Nothing

validExpirationYear :: String -> Maybe String
validExpirationYear s = do
  i <- readMaybe s :: Maybe Integer
  if i >= 2020 && i <= 2030 then Just s else Nothing

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left  a) = Nothing
eitherToMaybe (Right b) = Just b

validHeight :: String -> Maybe String
validHeight s = (eitherToMaybe $ parse heightParser "" s) >>= f
 where
  f =
    (\t ->
      if fst t == "cm" then validCmRange (snd t) else validInchRange (snd t)
    )

validCmRange :: String -> Maybe String
validCmRange s = do
  i <- readMaybe s :: Maybe Integer
  if i >= 150 && i <= 193 then Just s else Nothing

validInchRange :: String -> Maybe String
validInchRange s = do
  i <- readMaybe s :: Maybe Integer
  if i >= 59 && i <= 76 then Just s else Nothing

validHairColor :: String -> Maybe String
validHairColor s = eitherToMaybe $ parse hairParser "" s

validEyeColor :: String -> Maybe String
validEyeColor s = eitherToMaybe $ parse eyeParser "" s

validPassportId :: String -> Maybe String
validPassportId s = eitherToMaybe $ parse passportIdParser "" s

passportIdParser :: GenParser Char str String
passportIdParser = do
  pid <- count 9 digit
  eof
  return pid

eyeParser :: GenParser Char str String
eyeParser =
  try (string "amb")
    <|> try (string "blu")
    <|> try (string "brn")
    <|> try (string "grn")
    <|> try (string "gry")
    <|> try (string "hzl")
    <|> try (string "oth")

hairParser :: GenParser Char str String
hairParser = do
  char '#'
  hex <- count
    6
    (   digit
    <|> char 'a'
    <|> char 'b'
    <|> char 'c'
    <|> char 'd'
    <|> char 'e'
    <|> char 'f'
    )
  eof
  return ("#" ++ hex)

heightParser :: GenParser Char str (String, String)
heightParser = do
  ds   <- many digit
  unit <- string "cm" <|> string "in"
  return (unit, ds)


countMaybes :: [Maybe String] -> Integer
countMaybes ms = foldr
  (\m -> \x -> case m of
    Just val -> x + 1
    Nothing  -> x
  )
  0
  ms

dropEmpties :: [[(String, String)]] -> [[(String, String)]]
dropEmpties kvs = filter (\xs -> length kvs > 0) kvs

file :: (M.Map String String -> Maybe String) -> GenParser Char str Integer
file f = do
  lines         <- line `sepBy` (char '\n')
  passportCount <-
    return $ countMaybes $ fmap (f . M.fromList) $ dropEmpties lines
  eof
  return passportCount

line :: GenParser Char str [(String, String)]
line = do
  kvs <- many kv
  return kvs

kv :: GenParser Char str (String, String)
kv = do
  key <- many alphaNum
  char ':'
  val <- many (alphaNum <|> (char '#'))
  char ' ' <|> char '\n'
  return (key, val)
