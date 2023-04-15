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
    [] -> eerr (unexpectError "")
    (t:ts)
      | test t    -> cok t ts newErrorUnknown
      | otherwise -> eerr (unexpectError $ show [t])



char :: Char -> Parsel String Char
char c = satisfy (c ==)



string :: String -> Parsel String String
string [] = Parsel $ \s _ _ eok _ -> eok [] s newErrorUnknown
string tts@(t:ts) = Parsel $ \input cok cerr eok eerr ->
  let errEof = setErrorMessage (Expect $ show tts) $ unexpectError ""
      errUnexpected a = setErrorMessage (Expect $ show tts) $ unexpectError (show [a])

      walk []     rss = cok tts rss newErrorUnknown
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