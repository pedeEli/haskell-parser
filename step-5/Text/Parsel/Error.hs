module Text.Parsel.Error where

import Data.List (sort, nub)
import Text.Parsel.Pos
import Debug.Trace (trace)


data Message = SysUnExpect !String
             | UnExpect !String
             | Expect !String
             | Message !String

instance Enum Message where
  fromEnum (SysUnExpect _) = 0
  fromEnum (UnExpect    _) = 1
  fromEnum (Expect      _) = 2
  fromEnum (Message     _) = 3
  toEnum _ = error "toEnum is undefined for Message"

instance Eq Message where
  m1 == m2 = fromEnum m1 == fromEnum m2

instance Ord Message where
  compare m1 m2 = compare (fromEnum m1) (fromEnum m2)

messageString :: Message -> String
messageString (SysUnExpect t) = t
messageString (UnExpect    t) = t
messageString (Expect      t) = t
messageString (Message     t) = t



data ParseError = ParseError SourcePos [Message]


newErrorMessage :: Message -> SourcePos -> ParseError
newErrorMessage msg pos = ParseError pos [msg]

newErrorUnknown :: SourcePos -> ParseError
newErrorUnknown pos = ParseError pos []

errorIsUnknown :: ParseError -> Bool
errorIsUnknown (ParseError _ msgs) = null msgs

addErrorMessage :: Message -> ParseError -> ParseError
addErrorMessage msg (ParseError pos msgs) = ParseError pos (msg : msgs)

setErrorMessage :: Message -> ParseError -> ParseError
setErrorMessage msg (ParseError pos msgs)
  = ParseError pos (msg : filter (/= msg) msgs)

errorMessages :: ParseError -> [Message]
errorMessages (ParseError _ msgs) = sort msgs

errorPos :: ParseError -> SourcePos
errorPos (ParseError pos _) = pos

mergeError :: ParseError -> ParseError -> ParseError
mergeError e1@(ParseError pos1 msgs1) e2@(ParseError pos2 msgs2)
  | null msgs1 && not (null msgs2) = e2
  | null msgs2 && not (null msgs1) = e1
  | otherwise = case compare pos1 pos2 of
    EQ -> ParseError pos1 (msgs1 ++ msgs2)
    GT -> e1
    LT -> e2



instance Show ParseError where
  show err = show (errorPos err) ++ ":" ++
    showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages err)


showErrorMessages :: String -> String -> String -> String -> String -> [Message] -> String
showErrorMessages msgOr msgUnknown msgExpecting msgUnexpected msgEndOfInput msgs
  | null msgs = msgUnknown
  | otherwise = concatMap ("\n" ++) $ clean [showSysUnExpect, showUnExpect, showExpect, showMessages]
    where
      -- split the sorted msgs into the dirrerent kinds
      (sysUnExpect, msgs1) = span (SysUnExpect "" ==) msgs
      (unExpect, msgs2)    = span (UnExpect    "" ==) msgs1
      (expect, messages)   = span (Expect      "" ==) msgs2

      showExpect = showMany msgExpecting expect
      showUnExpect = showMany msgUnexpected unExpect
      showMessages = showMany "" messages
      showSysUnExpect
        | not (null unExpect)      = "" -- UnExpect overwrites SysUnExpect
        | null sysUnExpect         = ""
        | msg:_ <- sysUnExpect -- if the message in SysUnExpect is an empty string that means the end of input
        , null (messageString msg) = msgUnexpected ++ " " ++ msgEndOfInput -- was reached unexpected
        | msg:_ <- sysUnExpect     = msgUnexpected ++ " " ++ messageString msg

      
      -- concatinate multiple messages of the same kind together.
      -- [Expect "foo", Expect "bar", Expect "wow", Expect "damn"]
      -- would be converted to
      -- "expecting foo, bar, wow or damn"
      showMany pre ms = case clean $ map messageString ms of
        [] -> ""
        ms | null pre  -> commasOr ms
           | otherwise -> pre ++ " " ++ commasOr ms

      commasOr [] = ""
      commasOr [m] = m
      commasOr ms = seperate ", " (init ms) ++ " " ++ msgOr ++ " " ++ last ms

      seperate   _ []     = ""
      seperate   _ [m]    = m
      seperate sep (m:ms) = m ++ sep ++ seperate sep ms

      -- remove empty strings and duplicates
      clean = nub . filter (not . null)