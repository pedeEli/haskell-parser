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
which makes sense, a parser either succeeds or it fails but this misses a key detail. We have no idea if the parser has consumed any input or not. This feature is is extremly importent which we will see later down the road. Our actual definition is

```haskell
newtype Parsel s a = Parsel {
  unParsel :: forall b.
              s
           -> (a -> s -> ParseError -> b) -- consumed ok
           -> (ParseError -> b)           -- consumed error
           -> (a -> s -> ParseError -> b) -- empty ok
           -> (ParseError -> b)           -- empty error
           -> b}
```
where `ParseError` is defined like so
```haskell
type ParseError = [String]
```
