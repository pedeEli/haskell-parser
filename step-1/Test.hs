module Test (
  module Text.Parsel,
  satisfy,
  char,
  string
) where

import Text.Parsel


satisfy :: (Char -> Bool) -> Parsel String Char
satisfy test = Parsel $ \input cok cerr eok eerr ->
  case input of
    [] -> eerr ["unexepcted end of input"]
    (t:ts)
      | test t    -> cok t ts []
      | otherwise -> eerr ["unexpected " ++ show [t]]



char :: Char -> Parsel String Char
char c = satisfy (c ==)




string :: String -> Parsel String String
string [] = Parsel $ \s _ _ eok _ -> eok [] s []
string tts@(t:ts) = Parsel $ \input cok cerr eok eerr ->
  let errEof = ["expected " ++ show tts, "unexpected end of input"]
      errUnexpected a = ["expected " ++ show tts, "unexpected " ++ show [a]]

      walk []     rss = cok tts rss []
      walk (x:xs) rss = case rss of
        [] -> cerr errEof
        (r:rs)
          | x == r    -> walk xs rs
          | otherwise -> cerr (errUnexpected r)
  in case input of
    [] -> eerr errEof
    (r:rs)
      | r == t    -> walk ts rs
      | otherwise -> eerr (errUnexpected r)