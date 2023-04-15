{-# LANGUAGE FlexibleContexts #-}
module Text.Parsel.Char where

import Text.Parsel.Primitives
import Text.Parsel.Pos
import Data.Char (isSpace, isUpper, isLower, isAlphaNum, isLetter, isDigit, isHexDigit, isOctDigit)
import Control.Applicative


satisfy :: Stream s Char => (Char -> Bool) -> Parsel s Char
satisfy test = token (\c -> show [c])
                     updatePosChar
                     (\c -> if test c then Just c else Nothing)


char :: Stream s Char => Char -> Parsel s Char
char c = satisfy (c ==)


string :: Stream s Char => String -> Parsel s String
string = tokens show updatePosString


oneOf :: Stream s Char => [Char] -> Parsel s Char
oneOf cs = satisfy (`elem` cs)


noneOf :: Stream s Char => [Char] -> Parsel s Char
noneOf cs = satisfy (`notElem` cs)


space :: Stream s Char => Parsel s Char
space = satisfy isSpace <?> "space"


spaces :: Stream s Char => Parsel s ()
spaces = skipMany space <?> "white space"


newLine :: Stream s Char => Parsel s Char
newLine = char '\n' <?> "lf new-line"


crlf :: Stream s Char => Parsel s Char
crlf = char '\r' *> char '\n' <?> "crlf new-line"


endOfLine :: Stream s Char => Parsel s Char
endOfLine = newLine <|> crlf <?> "new-line"


tab :: Stream s Char => Parsel s Char
tab = char '\t' <?> "tab"


upper :: Stream s Char => Parsel s Char
upper = satisfy isUpper <?> "uppercase letter"


lower :: Stream s Char => Parsel s Char
lower = satisfy isLower <?> "lowercase letter"


alphaNum :: Stream s Char => Parsel s Char
alphaNum = satisfy isAlphaNum <?> "letter or digit"


letter :: Stream s Char => Parsel s Char
letter = satisfy isLetter <?> "letter"


digit :: Stream s Char => Parsel s Char
digit = satisfy isDigit <?> "digit"


hexDigit :: Stream s Char => Parsel s Char
hexDigit = satisfy isHexDigit <?> "hexadecimal digit"


octDigit :: Stream s Char => Parsel s Char
octDigit = satisfy isOctDigit <?> "octal digit"


anyChar :: Stream s Char => Parsel s Char
anyChar = satisfy (const True)