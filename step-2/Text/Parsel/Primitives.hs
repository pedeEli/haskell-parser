{-# LANGUAGE Rank2Types #-}
module Text.Parsel.Primitives where

import Control.Monad
import Control.Applicative
import Text.Parsel.Error

newtype Parsel s a = Parsel {
  unParsel :: forall b.
              s
           -> (a -> s -> ParseError -> b) -- consumed ok
           -> (ParseError -> b)                 -- consumed error
           -> (a -> s -> ParseError -> b) -- empty ok
           -> (ParseError -> b)                 -- empty error
           -> b}



runParsel :: Parsel s a -> s -> Either ParseError a
runParsel p s = unParsel p s cok cerr eok eerr
  where cok a s' err = Right a
        cerr = Left
        eok a s' err = Right a
        eerr = Left



unexpectError :: String -> ParseError
unexpectError msg = newErrorMessage (SysUnExpect msg)


instance Functor (Parsel s) where
  fmap f p = Parsel $ \s cok cerr eok eerr ->
    unParsel p s (cok . f) cerr (eok . f) eerr


instance Applicative (Parsel s) where
  pure a = Parsel $ \s _ _ eok _ -> eok a s newErrorUnknown
  (<*>) = ap

instance Alternative (Parsel s) where
  empty = Parsel $ \s _ _ _ eerr -> eerr newErrorUnknown
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