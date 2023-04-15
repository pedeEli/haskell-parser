# Step 1: Basic structure

Many people would design their parser like this
```haskell
newtype Parsel s a = Parsel {
  unParsel :: forall b.
           -> s
           -> (a -> s -> ParseError -> b) -- called when successfull
           -> (ParseError -> b)           -- called when failed
           -> b}
```
which makes sense, a parser either succeeds or it fails but this misses a key detail. We have no idea if the parser has consumed any input or not. This feature is is extremly important which we will see later down the road. Our actual definition is

```haskell
newtype Parsel s a = Parsel {
  unParsel :: forall b.
              s
           -> (a -> s -> ParseError -> b) -- consumed ok    (short cok)
           -> (ParseError -> b)           -- consumed error (short cerr)
           -> (a -> s -> ParseError -> b) -- empty ok       (short eok)
           -> (ParseError -> b)           -- empty error    (short eerr)
           -> b}
```
- The `ok` functions take the thing that was parsed (`a`), the new state (`s`), an error and produce a `b`. Having the error even in the ok case is usefull when combining different parsers. We will see that later.
- The `error` functions just take an error and produce a `b`.

`ParseError` is defined like that
```haskell
type ParseError = [String]
```
An empty list denotes an unknown error and is normaly what is passed to the `ok` functions.

## Basic Parsers
To make a simple Char parser we can simply write ([Test.hs](Test.hs))
```haskell
-- more generic version of a Char parser
satisfy :: (Char -> Bool) -> Parsel String Char
satisfy test = Parsel $ \input cok cerr eok eerr ->
  case input of
    [] -> eerr ["unexepcted end of input"] -- (1)
    (t:ts)
      | test t    -> cok t ts [] -- (2)
      | otherwise -> eerr ["unexpected " ++ show [t]] -- (1)

char :: Char -> Parsel String Char
char c = satisfy (c ==)
```
- (1) call to `eerr`: The parser failed at the beginning therefor no input was consumed.
- (2) call to `cok`: The parser succeded and it consumed input.

The implementation of a `String` parser is a bit more challenging  ([Test.hs](Test.hs))
```haskell
string :: String -> Parsel String String
string [] = Parsel $ \s _ _ eok _ -> eok [] s []
string tts@(t:ts) = Parsel $ \input cok cerr eok eerr ->
  let -- error helpers
      errEof = ["expected " ++ show tts, "unexpected end of input"]
      errUnexpected a = ["expected " ++ show tts, "unexpected " ++ show [a]]

      walk []     rss = cok tts rss [] -- (5)
      walk (x:xs) rss = case rss of
        [] -> cerr errEof -- (3)
        (r:rs)
          | x == r    -> walk xs rs -- (4)
          | otherwise -> cerr (errUnexpected r) -- (3)
  in case input of
    [] -> eerr errEof -- (1)
    (r:rs)
      | r == t    -> walk ts rs -- (2)
      | otherwise -> eerr (errUnexpected r) -- (1)
```
- (1) call to `eerr`: The parser failed at the beginning therefor no input was consumed.
- (2) call to `walk`: The first `Char` was parsed successfully therefor we can try and parse the rest. This is a seperate function so we know we started consuming input.
- (3) call to `cerr`: The parser failed and we already started consuming input.
- (4) call to `walk`: Recursion step.
- (5) call to `cok`: The parser succeded and it consumed input.


## Running Parsers
We define the `runParsel` function ([Primitives.hs](Text/Parsel/Primitives.hs))
```haskell
runParsel :: Parsel s a -> s -> Either ParseError a
runParsel p s = unParsel p s cok cerr eok eerr
  where cok a s' err = Right a
        cerr = Left
        eok a s' err = Right a
        eerr = Left
```
We don't really care if input was consumed or not, that is why the cok and eok, cerr and eerr are the same. But this doesn't has to be the case. I'm sure there is a usecase of knowing if the parser consumed input or not.

## Instances for Parsel
`Functor` is pretty straight forward
```haskell
instance Functor (Parsel s) where
  fmap f p = Parsel $ \s cok cerr eok eerr ->
    unParsel p s (cok . f) cerr (eok . f) eerr
```
`(cok . f)` and `(eok . f)` explained (pseudo haskell)
```haskell
p :: Parsel s a
cok :: b -> s -> ParseError -> c
f :: a -> b
\a -> cok (f a) :: a -> s -> ParseError -> c
\a -> cok (f a) = \a -> cok $ f a
                = \a -> (cok . f) a 
                = cok . f
```
`Applicative` is also easy because we can use a function from `Control.Monad`
```haskell
instance Applicative (Parsel s) where
  pure a = Parsel $ \s _ _ eok _ -> eok a s []
    -- did not fail and did not consume any input -> eok
  (<*>) = ap -- only works when (Parsel s) is a Monad
```
`Alternative` get's a bit more tricky because we want to concatinate some errors
```haskell
instance Alternative (Parsel s) where
  empty = Parsel $ \s _ _ _ eerr -> eerr []
  m <|> n = Parsel $ \s cok cerr eok eerr ->
    let meerr err =
          let neok a s err' = eok a s (err ++ err') -- (2)
              neerr err' = eerr (err ++ err') -- (2)
          in unParsel n s cok cerr neok neerr
    in unParsel m s cok cerr eok meerr -- (1)
```
- (1) only when the first parser `m` fails without consuming anything the second parser is tried. That is why we modify the `eerr` function to be `meerr`.
- (2) we concatinate only the previous error when the second parser has not consumed any thing. We We don't need to know the error from `m` in `cerr`. This is more obvious with an example. Suppose where have to parsers and combine them with `<|>`
```haskell
emptyFail = Parsel $ \s _ _ _ eerr = eerr ["i am always failing"]
hw = string "hello world"
combined = emptyFail <|> hw

example1 = runParsel combined "hello world" -- succeeds
example2 = runParsel combined "hello " -- fails
```
If we would combine the errors on `cerr` (that happends in `example`) we would get the error messages
```haskell
example2 = Left [
  "i am always failing",
  "expected \"hello world\"",
  "unexpected end of input"]
```
It makes no sense seeing `"i am always failing` here and that is why we only combine errors on `eok` and `eerr`

`Monad` is for the same reasons tricky as `Alternative`
```haskell
instance Monad (Parsel s) where
  m >>= f = Parsel $ \s cok cerr eok eerr ->
    let mcok a s err -- (2)
          | null err = unParsel (f a) s cok cerr cok cerr
          | otherwise =
            let feok b s err' = cok b s (err ++ err')
                feerr err' = cerr (err ++ err')
            in unParsel (f a) s cok cerr feok feerr
        meok a s err -- (3)
          | null err = unParsel (f a) s cok cerr eok eerr
          | otherwise =
            let feok b s err' = eok b s (err ++ err')
                feerr err' = eerr (err ++ err')
            in unParsel (f a) s cok cerr feok feerr
    in unParsel m s mcok cerr meok eerr -- (1)
```
- (1) error are send through, no suprise here. Only when an output is produces we can evaluate `f`. Though extremly similar `mcok` and `meok` are different.
- (2) both cases are basically the same. When the error is unknown there is just no need to combine any errors. For the same reason as in `Alternative` we only combine when `(f a)` does not consume any input. You might saw that we are using the `cok` function in `feok`. Because `mcok` is only called when `m` consumed any input we know that `m >>= f` consumed for sure something. So even if `(f a)` does not consume anything we still call `cok`. Same is true for `feerr` and `eerr`.
- (3) same reasoning as in (2) only this time we know `m` did not consume anything. This means we use `eok` in `feok` and `eerr` in `feerr`.

## Problem with `<|>`
The following code does not what you would expect
```haskell
letterOrLemon = string "letter" <|> string "lemon"
example = runParsel letterOrLemon "lemon"
```
`example` fails the the error message
```haskell
example = Left ["expected \"letter\"", "unexpected \"m\""]
```
But why. Shouldn't `<|>` run `string "lemon"` when `string "letter"` fails? Yes but only if `string "letter"` does not consume any input. In our example it consumes `"le"`. This means we need a way to make a parser never consume any input. That is exacly what `try` does
```haskell
try :: Parsel s a -> Parsel s a
try p = Parsel $ \s _ _ eok eerr -> unParsel p s eok eerr eok eerr
```
The trick is to provide `eok` were `cok` would go and `eerr` where `cerr` would go. To make our example work all we need to do now is
```haskell
letterOrLemon = try (string "letter") <|> string "lemon"
example = runParsel letterOrLemon "lemon"
```
`example` is successfull this time, because even though `string "letter"` fails with a consumed error (`cerr`) the `try` transformer calls instead empty error (`eerr`)

## Final words
This was the beginning of our implementation. It is very simple but already kind of powerfull. Our error messages kind of suck though so let's fix that in [step-2](/step-2).