{-# LANGUAGE Rank2Types, FunctionalDependencies, FlexibleInstances #-}
module Text.Parsel.Primitives where

import Control.Monad
import Control.Applicative
import Text.Parsel.Error
import Text.Parsel.Pos

newtype Parsel s a = Parsel {
  unParsel :: forall b.
              State s
           -> (a -> State s -> ParseError -> b) -- consumed ok
           -> (ParseError -> b)                 -- consumed error
           -> (a -> State s -> ParseError -> b) -- empty ok
           -> (ParseError -> b)                 -- empty error
           -> b}


data State s = State {
  stateInput :: s,
  statePos :: SourcePos}


runParsel :: Parsel s a -> SourceName -> s -> Either ParseError a
runParsel p name s = unParsel p (State s (initialPos name)) cok cerr eok eerr
  where cok a s' err = Right a
        cerr = Left
        eok a s' err = Right a
        eerr = Left



unknownError :: State s -> ParseError
unknownError = newErrorUnknown . statePos

unexpectError :: String -> State s -> ParseError
unexpectError msg = newErrorMessage (SysUnExpect msg) . statePos


instance Functor (Parsel s) where
  fmap f p = Parsel $ \s cok cerr eok eerr ->
    unParsel p s (cok . f) cerr (eok . f) eerr


instance Applicative (Parsel s) where
  pure a = Parsel $ \s _ _ eok _ -> eok a s (unknownError s)
  (<*>) = ap

instance Alternative (Parsel s) where
  empty = Parsel $ \s _ _ _ eerr -> eerr (unknownError s)
  m <|> n = Parsel $ \s cok cerr eok eerr ->
    let meerr err =
          let neok a s err' = eok a s (mergeError err err')
              neerr err' = eerr (mergeError err err')
          in unParsel n s cok cerr neok neerr
    in unParsel m s cok cerr eok meerr

  
instance Monad (Parsel s) where
  m >>= f = Parsel $ \s cok cerr eok eerr ->
    let mcok a s err
          | errorIsUnknown err = unParsel (f a) s cok cerr cok cerr
          | otherwise =
            let feok b s err' = cok b s (mergeError err err')
                feerr err' = cerr (mergeError err err')
            in unParsel (f a) s cok cerr feok feerr
        meok a s err
          | errorIsUnknown err = unParsel (f a) s cok cerr eok eerr
          | otherwise =
            let feok b s err' = eok b s (mergeError err err')
                feerr err' = eerr (mergeError err err')
            in unParsel (f a) s cok cerr feok feerr
    in unParsel m s mcok cerr meok eerr



try :: Parsel s a -> Parsel s a
try p = Parsel $ \s _ _ eok eerr -> unParsel p s eok eerr eok eerr


infix 0 <?>
(<?>) :: Parsel s a -> String -> Parsel s a
(<?>) = label

label :: Parsel s a -> String -> Parsel s a
label p msg = labels p [msg]

labels :: Parsel s a -> [String] -> Parsel s a
labels p msgs = Parsel $ \s cok cerr eok eerr ->
    let eok' a s err = eok a s $ if errorIsUnknown err
          then err
          else setExpectErrors err msgs
        eerr' err = eerr $ setExpectErrors err msgs
    in unParsel p s cok cerr eok' eerr'
  where
    setExpectErrors err [] = setErrorMessage (Expect "") err
    setExpectErrors err [msg] = setErrorMessage (Expect msg) err
    setExpectErrors err (msg:msgs)
      = foldr (addErrorMessage . Expect) (setErrorMessage (Expect msg) err) msgs



class Stream s t | s -> t where
  uncons :: s -> Maybe (t, s)

instance Stream String Char where
  uncons [] = Nothing
  uncons (t:ts) = Just (t, ts)


token :: Stream s t
      => (t -> String)
      -> (SourcePos -> t -> SourcePos)
      -> (t -> Maybe a)
      -> Parsel s a
token showToken nextPos test = Parsel $ \s@(State input pos) cok cerr eok eerr ->
  case uncons input of
    Nothing -> eerr (unexpectError "" s)
    Just (t, ts) -> case test t of
      Nothing -> eerr (unexpectError (showToken t) s)
      Just a ->
        let newPos = nextPos pos t
            newState = State ts newPos
        in cok a newState (unknownError newState)

tokens :: (Stream s t, Eq t)
       => ([t] -> String)
       -> (SourcePos -> [t] -> SourcePos)
       -> [t]
       -> Parsel s [t]
tokens _ _ [] = Parsel $ \s _ _ eok _ -> eok [] s (unknownError s)
tokens showTokens nextPos tts@(t:ts) = Parsel $ \s@(State input pos) cok cerr _ eerr ->
  let errEof = setErrorMessage (Expect (showTokens tts)) (unexpectError "" s)
      errUnexpected t = setErrorMessage (Expect (showTokens tts)) (unexpectError (showTokens [t]) s)

      walk [] rrs =
        let newPos = nextPos pos tts
            newState = State rrs newPos
        in cok tts newState (unknownError newState)
      walk (t:ts) rrs = case uncons rrs of
        Nothing -> cerr errEof
        Just (r, rs)
          | t == r    -> walk ts rs
          | otherwise -> cerr (errUnexpected r)

  in case uncons input of
    Nothing -> eerr errEof
    Just (r, rs)
      | r == t    -> walk ts rs
      | otherwise -> eerr (errUnexpected r)