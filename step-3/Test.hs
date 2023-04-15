module Test (
  module Text.Parsel,
  satisfy,
  char,
  string
) where

import Text.Parsel


satisfy :: (Char -> Bool) -> Parsel String Char
satisfy test = Parsel $ \s@(State input pos) cok cerr eok eerr ->
  case input of
    [] -> eerr (unexpectError "" s)
    (t:ts)
      | test t    -> let newPos = updatePosChar pos t
                         newState = State ts newPos 
                     in cok t newState (unknownError newState)
      | otherwise -> eerr (unexpectError (show [t]) s)



char :: Char -> Parsel String Char
char c = satisfy (c ==)



string :: String -> Parsel String String
string [] = Parsel $ \s _ _ eok _ -> eok [] s (unknownError s)
string tts@(t:ts) = Parsel $ \s@(State input pos) cok cerr eok eerr ->
  let errEof = setErrorMessage (Expect (show tts)) (unexpectError "" s)
      errUnexpected a = setErrorMessage (Expect (show tts)) (unexpectError (show [a]) s)

      walk []     rrs =
        let newPos = updatePosString pos tts
            newState = State rrs newPos
        in cok tts newState (unknownError newState)
      walk (t:ts) rss = case rss of
        [] -> cerr errEof
        (r:rs)
          | t == r    -> walk ts rs
          | otherwise -> cerr (errUnexpected r)
  in case input of
    [] -> eerr errEof
    (r:rs)
      | r == t    -> walk ts rs
      | otherwise -> eerr (errUnexpected r)