{-# LANGUAGE FlexibleContexts #-}
module Text.Parsel.Char where

import Text.Parsel.Primitives
import Text.Parsel.Pos


satisfy :: Stream s Char => (Char -> Bool) -> Parsel s Char
satisfy test = token (\c -> show [c])
                     updatePosChar
                     (\c -> if test c then Just c else Nothing)


char :: Stream s Char => Char -> Parsel s Char
char c = satisfy (c ==)


string :: Stream s Char => String -> Parsel s String
string = tokens show updatePosString