module Text.Parsel.Error where
import Data.List (sort, nub)



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



newtype ParseError = ParseError [Message]


newErrorMessage :: Message -> ParseError
newErrorMessage msg = ParseError [msg]

newErrorUnknown :: ParseError
newErrorUnknown = ParseError []

errorIsUnknown :: ParseError -> Bool
errorIsUnknown (ParseError msgs) = null msgs

addErrorMessage :: Message -> ParseError -> ParseError
addErrorMessage msg (ParseError msgs) = ParseError (msg : msgs)

setErrorMessage :: Message -> ParseError -> ParseError
setErrorMessage msg (ParseError msgs)
  = ParseError (msg : filter (/= msg) msgs)

errorMessages :: ParseError -> [Message]
errorMessages (ParseError msgs) = sort msgs

mergeError :: ParseError -> ParseError -> ParseError
mergeError (ParseError msgs1) (ParseError msgs2) = ParseError (msgs1 ++ msgs2)



instance Show ParseError where
  show err = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages err)


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