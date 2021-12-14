module Token where

import Control.Applicative (liftA2)
import Data.Foldable (asum)
import Data.Maybe (isNothing, fromJust)
import Data.Char
import Text.Parsec as P
import Structure


literal :: Parsec String () String
literal = float <|> integer <|> Token.char <|> Token.string

special :: Parsec String () Char
special = oneOf "(),;[]`{}"
          
whitespace :: Parsec String () String
whitespace = concat <$> many1 whitestuff

whitestuff :: Parsec String () String
whitestuff = fmap (:"") whitechar <|> try comment <|> try ncomment <?> "whitespace"

whitechar :: Parsec String () Char
whitechar = Token.newline <|> vertab <|> Token.space <|> Token.tab <|> uniWhite

newline :: Parsec String () Char
newline = (returnc *> linefeed) <|> returnc <|> linefeed <|> formfeed

returnc :: Parsec String () Char
returnc = P.char '\r'

linefeed :: Parsec String () Char
linefeed = P.char '\n'

vertab :: Parsec String () Char
vertab = P.char '\v'

formfeed :: Parsec String () Char
formfeed = P.char '\f'

space :: Parsec String () Char
space = P.char ' '

tab :: Parsec String () Char
tab = P.char '\t'

uniWhite :: Parsec String () Char
uniWhite = satisfy isSpace

comment :: Parsec String () String
comment = do
        dashes
        com <- option "" $ (anyc `exclude` symbol) *> many anyc
        return $ "--" ++ com ++ "\n"

dashes :: Parsec String () String
dashes = liftA2 (++) (P.string "--") $ many (P.char '-')

opencom :: Parsec String () String
opencom = P.string "{-"

closecom :: Parsec String () String
closecom = P.string "-}"

ncomment :: Parsec String () String
ncomment = do
        opencom
        com <- aNYseq
        inner <- option "" $ liftA2 (++) ncomment aNYseq
        closecom
        return $ "{-" ++ com ++ inner ++ "-}"

aNYseq :: Parsec String () String
aNYseq = manyTill aNY $ lookAhead $ P.string "-}" <|> P.string "{-"

aNY :: Parsec String () Char
aNY = graphic <|> whitechar

anyc :: Parsec String () Char
anyc = graphic <|> Token.space <|> Token.tab

graphic :: Parsec String () Char
graphic = small <|> large <|> symbol <|> Token.digit <|> special <|> P.char ':' <|> P.char '"' <|> P.char '\''

small :: Parsec String () Char
small = ascSmall <|> uniSmall <|> P.char '_'

ascSmall :: Parsec String () Char
ascSmall = satisfy isAsciiLower

uniSmall :: Parsec String () Char
uniSmall = satisfy isLower

large :: Parsec String () Char
large = ascLarge <|> uniLarge

ascLarge :: Parsec String () Char
ascLarge = satisfy isAsciiUpper

uniLarge :: Parsec String () Char
uniLarge = satisfy isUpper

symbol :: Parsec String () Char
symbol = let ext = special <|> P.char '_' <|> P.char ':' <|> P.char '"' <|> P.char '\''
         in ascSymbol <|> uniSymbol `exclude` ext

ascSymbol :: Parsec String () Char
ascSymbol = oneOf "!#$%&*+./<=>?@\\^|-"

uniSymbol :: Parsec String () Char
uniSymbol = satisfy isSymbol

digit :: Parsec String () Char
digit = ascDigit <|> uniDigit

ascDigit :: Parsec String () Char
ascDigit = oneOf "0123456789"

uniDigit :: Parsec String () Char
uniDigit = satisfy isDigit

octit :: Parsec String () Char
octit = oneOf "01234567" 

hexit :: Parsec String () Char
hexit = Token.digit <|> oneOf "abcdefABCDEF"


varid :: Parsec String () Token
varid = toToken $ liftA2 (:) small (many $ small <|> large <|> Token.digit <|> P.char '\'') `exclude` reservedid

conid :: Parsec String () Token
conid = toToken $ liftA2 (:) large $ many (small <|> large <|> Token.digit <|> P.char '\'')

reservedid :: Parsec String () String
reservedid = let ids = ["case", "class", "data", "default", "deriving", "do", "else",
                        "if", "import", "in", "infix", "infixl", "infixr", "instance",
                        "let", "module", "newtype", "of", "then", "type", "where", "_"]
             in asum $ map P.string ids

varsym :: Parsec String () Token
varsym = toToken $ liftA2 (:) symbol (many (symbol <|> P.char ':')) `exclude` (reservedop <|> dashes)

consym :: Parsec String () Token
consym = toToken $ liftA2 (:) (P.char ':') (many (symbol <|> P.char ':')) `exclude` reservedop

reservedop :: Parsec String () String
reservedop = let ops = ["..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"]
             in asum $ map P.string ops

modid :: Parsec String () Token
modid = conid

qparser :: Parsec String () Token -> Parsec String () Token
qparser p = do
        start <- getPosition
        Token v _ _ _ _ <- p
        mod <- many $ try $ do
            P.string "."
            Token str _ _ _ _ <- modid <?> "module name"
            return $ "." ++ str
        end <- getPosition
        return $ Token (v ++ concat mod) (sourceColumn start) (sourceColumn end) (sourceLine start) (sourceLine end)

qvarid :: Parsec String () Token
qvarid = qparser varid

qvarsym :: Parsec String () Token
qvarsym = qparser varsym

qconid :: Parsec String () Token
qconid = qparser conid

qconsym :: Parsec String () Token
qconsym = qparser consym

decimal :: Parsec String () String
decimal = many1 Token.digit

octal :: Parsec String () String
octal = many1 octit

hexadecimal :: Parsec String () String
hexadecimal = many1 hexit

integer :: Parsec String () String
integer = decimal
      <|> liftA2 (++) (P.string "0o") octal
      <|> liftA2 (++) (P.string "0O") octal
      <|> liftA2 (++) (P.string "0x") hexadecimal
      <|> liftA2 (++) (P.string "0X") hexadecimal

float :: Parsec String () String
float = try withDot <|> without
    where withDot = do
              start <- decimal
              P.char '.'
              end <- decimal
              exp <- option "" exponents
              return $ start ++ "." ++ end ++ exp    
          without = do
              start <- decimal
              exp <- exponents
              return $ start ++ exp

exponents :: Parsec String () String
exponents = do
    e <- P.char 'e' <|> P.char 'E'
    sign <- option "" $ P.string "+" <|> P.string "-"
    d <- decimal
    return $ e : sign ++ d

char :: Parsec String () String
char = do
    let converted = asum [graphic `exclude` (P.char '\'' <|> P.char '\\'), Token.space]
    P.char '\''
    c <- escape `exclude` P.string "\&" <|> (:"") <$> converted
    P.char '\''
    return $ "'" ++ c ++ "'"


string :: Parsec String () String
string = do
    let converted = (:"") <$> asum [graphic `exclude`( P.char '\"' <|> P.char '\\'), Token.space]
    P.char '"'
    s <- many $ escape <|> gap <|> converted
    P.char '"'
    return $ "\"" ++ concat s ++ "\""

escape :: Parsec String () String
escape = do
    let ooctal = (:) <$> P.char 'o' <*> octal
        xhexadecimal = (:) <$> P.char 'x' <*> hexadecimal
    P.char '\\'
    e <- charesc <|> ascii <|> decimal <|> ooctal <|> xhexadecimal
    return $ "\\" ++ e

charesc :: Parsec String () String
charesc = (:"") <$> oneOf "abfnrtv\\\"'&"

ascii :: Parsec String () String
ascii = asum (map P.string ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
                              "BS", "HT", "LF", "VT", "FF", "CR", "SO", "SI", "DLE",
                              "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB", "CAN",
                              "EM", "SUB", "ESC", "FS", "GS", "RS", "US", "SP", "DEL"]
        ) <|> c
    where c = liftA2 (:) (P.char '^') cntrl

cntrl :: Parsec String () String
cntrl = fmap (:"") $ ascLarge <|> asum (map P.char "@[\\]^_")

gap :: Parsec String () String
gap = do
    P.char '\\'
    start <- whitechar
    end <- many whitechar
    P.char '\\'
    return $ "\\" ++ [start] ++ end ++ "\\"


inParentheses :: Parsec String () a -> Parsec String () a
inParentheses p = do
    P.char '('
    optional whitespace
    token <- p
    optional whitespace
    P.char ')'
    return token

inBackticks :: Parsec String () a -> Parsec String () a
inBackticks p = do
    P.char '`'
    optional whitespace
    token <- p
    optional whitespace
    P.char '`'
    return token


var, qvar, con, qcon, varop, qvarop, conop, qconop, op, qop :: Parsec String () Token
var = varid <|> inParentheses varsym
qvar = qvarid <|> inParentheses qvarsym
con = conid <|> inParentheses consym
qcon = qconid <|> inParentheses qconsym
varop = varsym <|> inBackticks varid
qvarop = qvarsym <|> inBackticks qvarid
conop = consym <|> inBackticks conid
qconop = qconsym <|> inBackticks qconid
op = varop <|> conop
qop = qvarop <|> qconop



toToken :: Parsec String () String -> Parsec String () Token
toToken p = do
    start <- getPosition
    str <- p
    end <- getPosition
    return $ Token str (sourceColumn start) (sourceColumn end) (sourceLine start) (sourceLine end)


exclude :: (Eq a, Stream s m t) => ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
exclude p1 p2 = do
    a1 <- try $ lookAhead p1
    a2 <- optionMaybe $ try $ lookAhead p2
    if isNothing a2 || a1 /= fromJust a2 then p1 else parserZero


block :: String -> String -> String -> Parsec String () a -> Parsec String () [a]
block left right sep item = do
    let seperator = do
            optional whitespace
            P.string sep
            optional whitespace
    P.string left
    optional whitespace
    items <- item `sepEndBy` try seperator
    optional whitespace
    P.string right
    return items

curlyBracketBlock :: Parsec String () a -> Parsec String () [a]
curlyBracketBlock = block "{" "}" ";"

parenthesesBlock :: Parsec String () a -> Parsec String () [a]
parenthesesBlock = block "(" ")" ","