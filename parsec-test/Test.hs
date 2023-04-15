module Test where


import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Pos
import Debug.Trace




hello :: Parsec String () String
hello = string "hello "


world :: Parsec String () String
world = try $ string "world"




run :: (Stream s m t)
      => ParsecT s () m a -> s -> m (Either ParseError a)
run p s
    = do res <- runParsecT p (State s (initialPos "test") ())
         r <- parserReply res
         case r of
           Ok x _ _  -> trace "ok" $ return (Right x)
           Error err -> trace "error" $ return (Left err)
    where
        parserReply res
            = case res of
                Consumed r -> trace "consumed" r
                Empty    r -> trace "empty" r