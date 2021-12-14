{-# LANGUAGE TupleSections #-}
module TopLevel where

import Text.Parsec as P
import Data.Maybe (isJust)
import Structure
import Token


pmodule :: Parsec String () Module
pmodule = do
    let pnamed = do
        optional whitespace
        P.string "module"
        whitespace
        name <- qconid <?> "module name"
        optional whitespace
        es <- optionMaybe exports
        optional whitespace
        P.string "where" <?> "where"
        optional whitespace
        return (name, es)
    named <- optionMaybe pnamed
    b <- body
    case named of
        Just (name, es) -> return $ Named name es b
        Nothing -> return $ Main b

exports :: Parsec String () [Export]
exports = parenthesesBlock export

export :: Parsec String () Export
export =
    let expQVar = Exp'QVar <$> qvar
        expQCon = impExpCon Exp'QCon qconid
        expModule = do
            P.string "module"
            whitespace
            Exp'Module <$> qconid
    in expQVar <|> expQCon <|> expModule

impExpCon :: (Token -> Maybe [Token] -> a) -> Parsec String () Token -> Parsec String () a
impExpCon con pname = do
    name <- pname
    values <- optionMaybe $ try impExpConValues
    case values of
        Nothing -> return $ con name $ Just []
        Just vs -> return $ con name vs

impExpConValues :: Parsec String () (Maybe [Token])
impExpConValues = 
    let all = Nothing <$ try (inParentheses $ P.string "..")
        specific = fmap Just $ try $ parenthesesBlock $ var <|> con
    in all <|> specific <?> "variable"


body :: Parsec String () Body
body = (`Body` []) <$> curlyBracketBlock imports


imports :: Parsec String () Import
imports = do
    P.string "import"
    whitespace
    qualified <- optionMaybe $ P.string "qualified" <* whitespace
    name <- qconid
    alias <- optionMaybe $ try $ whitespace *> P.string "as" *> whitespace *> qconid
    let normal = optional whitespace *> importSpecs
        hiding = whitespace *> P.string "hiding" *> optional whitespace *> normal
    specs <- optionMaybe $ fmap (True,) hiding <|> fmap (False,) normal
    return $ Import name (isJust qualified) alias specs

importSpecs :: Parsec String () [ImportSpec]
importSpecs = parenthesesBlock importSpec

importSpec :: Parsec String () ImportSpec
importSpec =
    let impVar = Imp'Var <$> var
        impCon = impExpCon Imp'Con conid
    in impVar <|> impCon