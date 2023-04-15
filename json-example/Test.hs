module Test (
  module Text.Parsel,
  (<|>),
  JSON(..),
  jsonNull,
  jsonBool,
  jsonNumber,
  jsonString,
  jsonArray,
  jsonObject,
  parseJsonFile
) where

import Text.Parsel
import Control.Applicative ((<|>), asum)
import Data.Functor
import Data.List
import Data.Char
import Control.Monad

import Data.Map (Map)
import Data.Vector (Vector)
import Data.Scientific (Scientific)
import Data.Text (Text)

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Scientific as S
import qualified Data.Text as T
import Debug.Trace (traceM)


type Array = Vector JSON
type Object = Map Text JSON
data JSON = JsonNull
          | JsonBool Bool
          | JsonNumber Scientific
          | JsonString Text
          | JsonArray Array
          | JsonObject Object
  deriving (Show)

prettyShow :: JSON -> String
prettyShow JsonNull = "null"
prettyShow (JsonBool True) = "true"
prettyShow (JsonBool False) = "false"
prettyShow (JsonNumber s) = show s
prettyShow (JsonString s) = show s
prettyShow (JsonArray arr) = "[\n" ++ unlines (map ("  " ++) $ lines values) ++ "]"
  where values = intercalate ",\n" $ V.toList $ fmap prettyShow arr
prettyShow (JsonObject obj) = "{\n" ++ unlines (map ("  " ++) $ lines pairs) ++ "}"
  where pairs = intercalate ",\n" $ map (\(key, value) -> show key ++ ": " ++ prettyShow value) $ M.toList obj



jsonNull :: Parsel String JSON
jsonNull = string "null" $> JsonNull


jsonBool :: Parsel String JSON
jsonBool = (string "true"  $> JsonBool True) <|>
           (string "false" $> JsonBool False)


jsonNumber :: Parsel String JSON
jsonNumber = do
  sign <- option "" (string "-")

  digits <- many1 digit
  when (head digits == '0') $ unexpected "0"

  decimal <- option "" decimalP
  exponent <- option "" exponentP

  return $ JsonNumber $ read $ sign ++ digits ++ decimal ++ exponent
  where
    decimalP = do
      char '.'
      digits <- many1 digit
      return $ '.' : digits
    exponentP = do
      e <- oneOf "eE"
      sign <- option "" (singleton <$> oneOf "+-")
      digits <- many1 digit
      return $ e : sign ++ digits


literal :: Parsel String String
literal = concat <$> (char '"' *> many l <* char '"')
  where l = asum (map try [
              string "\\u" *> (singleton . toEnum . calcHex <$> replicateM 4 hexDigit),
              string "\\n" $> "\n",
              string "\\r" $> "\r",
              string "\\t" $> "\t",
              string "\\b" $> "\b",
              string "\\f" $> "\f",
              string "\\/" $> "/",
              string "\\\\" $> "\\",
              string "\\\"" $> "\"",
              string " "]) <|>
              (singleton <$> letter)

        calcHex [] = 0
        calcHex [c] = fromHex c
        calcHex (c:cs) = fromHex c * 16 + calcHex cs

        fromHex c
          | isDigit c = fromEnum c - fromEnum '0'
          | isHexDigit c && isLower c = fromEnum c - fromEnum 'a' + 10
          | isHexDigit c && isUpper c = fromEnum c - fromEnum 'A' + 10


jsonString :: Parsel String JSON
jsonString = JsonString . T.pack <$> literal


jsonArray :: Parsel String JSON
jsonArray = do
  char '['
  spaces
  arr <- sepBy jsonValue (char ',')
  char ']'
  return $ JsonArray $ V.fromList arr


jsonObject :: Parsel String JSON
jsonObject = do
  char '{'
  spaces
  let pair = do
        key <- literal
        spaces
        char ':'
        value <- jsonValue
        return (T.pack key, value)
  obj <- sepBy pair (char ',' *> spaces)
  char '}'
  return $ JsonObject $ M.fromList obj


jsonValue :: Parsel String JSON
jsonValue = spaces *> asum values <* spaces
  where values = map try [
          jsonNull,
          jsonBool,
          jsonNumber,
          jsonString,
          jsonArray,
          jsonObject]


json :: Parsel String JSON
json = jsonValue <* eof



parseJsonFile :: FilePath -> Bool -> IO ()
parseJsonFile path pretty = do
  file <- readFile path
  let result = runParsel json path file
  case result of
    Left err -> print err
    Right value -> if pretty
      then putStrLn $ prettyShow value
      else print value