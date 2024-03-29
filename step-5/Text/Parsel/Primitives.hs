{-# LANGUAGE Rank2Types, FunctionalDependencies, FlexibleInstances #-}
module Text.Parsel.Primitives where

import Control.Monad
import Data.Functor
import Control.Applicative (Alternative((<|>), empty))
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

getPos :: Parsel s SourcePos
getPos = Parsel $ \s@(State _ pos) _ _ eok _ -> eok pos s (unknownError s)


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




manyAccum :: (a -> [a] -> [a]) -> Parsel s a -> Parsel s [a]
manyAccum acc p = Parsel $ \s cok cerr eok eerr ->
  let walk as a s err = unParsel p s
        (walk (acc a as))
        cerr
        manyErr
        (eok (acc a as) s)
  in unParsel p s (walk []) cerr manyErr (eok [] s)

manyErr :: a
manyErr = error "Text.Parsel.Primitive.many: combinator many is applied to a parser that accepts an empty string"

many :: Parsel s a -> Parsel s [a]
many = fmap reverse . manyAccum (:)

many1 :: Parsel s a -> Parsel s [a]
many1 p = do
  a <- p
  as <- many p
  return (a:as)

skipMany :: Parsel s a -> Parsel s ()
skipMany = void . manyAccum (\_ _ -> [])



unexpected :: String -> Parsel s a
unexpected msg = Parsel $ \(State _ pos) _ _ _ eerr -> eerr (newErrorMessage (UnExpect msg) pos)


notFollowedBy :: Show a => Parsel s a -> Parsel s ()
notFollowedBy p = test <|> return ()
  where test = do
          s <- try p
          unexpected (show s)


anyToken :: (Stream s t, Show t) => Parsel s t
anyToken = token show const Just


eof :: (Stream s t, Show t) => Parsel s ()
eof = notFollowedBy anyToken <?> "end of input"


lookAhead :: Parsel s a -> Parsel s a
lookAhead p = Parsel $ \s _ cerr eok eerr ->
  let eok' a _ _ = eok a s (unknownError s)
  in unParsel p s eok' cerr eok' eerr


option :: a -> Parsel s a -> Parsel s a
option a p = p <|> return a


optionMaybe :: Parsel s a -> Parsel s (Maybe a)
optionMaybe p = option Nothing (Just <$> p)


optional :: Parsel s a -> Parsel s ()
optional p = (p $> ()) <|> return ()


sepBy1 :: Parsel s a -> Parsel s sep -> Parsel s [a]
sepBy1 p sep = do
  a <- p
  as <- many (sep >> p)
  return (a:as) 


sepBy :: Parsel s a -> Parsel s sep -> Parsel s [a]
sepBy p sep = sepBy1 p sep <|> return []