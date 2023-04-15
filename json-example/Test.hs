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
data JSON = JsonNull              SourcePos
          | JsonBool   Bool       SourcePos
          | JsonNumber Scientific SourcePos 
          | JsonString Text       SourcePos 
          | JsonArray  Array      SourcePos 
          | JsonObject Object     SourcePos 
  deriving (Show)

prettyShow :: JSON -> String
prettyShow (JsonNull _) = "null"
prettyShow (JsonBool True _) = "true"
prettyShow (JsonBool False _) = "false"
prettyShow (JsonNumber s _) = show s
prettyShow (JsonString s _) = show s
prettyShow (JsonArray arr _) = "[\n" ++ unlines (map ("  " ++) $ lines values) ++ "]"
  where values = intercalate ",\n" $ V.toList $ fmap prettyShow arr
prettyShow (JsonObject obj _) = "{\n" ++ unlines (map ("  " ++) $ lines pairs) ++ "}"
  where pairs = intercalate ",\n" $ map (\(key, value) -> show key ++ ": " ++ prettyShow value) $ M.toList obj




jsonNull :: Parsel String (SourcePos -> JSON)
jsonNull = string "null" $> JsonNull


jsonBool :: Parsel String (SourcePos -> JSON)
jsonBool = (string "true"  $> JsonBool True) <|>
           (string "false" $> JsonBool False)


jsonNumber :: Parsel String (SourcePos -> JSON)
jsonNumber = do
  sign <- option "" (string "-")

  digits <- many1 digit
  when (head digits == '0') $ unexpected "0"

  decimal <- option "" decimalP
  exponent <- option "" exponentP

  let scientific = read (sign ++ digits ++ decimal ++ exponent)
  return (JsonNumber scientific)
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


jsonString :: Parsel String (SourcePos -> JSON)
jsonString = JsonString . T.pack <$> literal


jsonArray :: Parsel String (SourcePos -> JSON)
jsonArray = do
  char '['
  spaces
  arr <- sepBy jsonValue (char ',')
  char ']'
  let vector = V.fromList arr
  return (JsonArray vector)


jsonObject :: Parsel String (SourcePos -> JSON)
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
  let map = M.fromList obj
  return (JsonObject map)


jsonValue :: Parsel String JSON
jsonValue = do
  spaces
  pos <- getPos
  value <- asum values
  spaces
  return (value pos)
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