module Parser
where
import Text.ParserCombinators.Parsec
import Scanner

type DecafParser a = GenParser Token () a

dtoken :: (DecafToken -> Maybe a) -> DecafParser a
dtoken test = token showTok posTok testTok
              where
                showTok (p1, p2, t) = show t
                posTok (p1, p2, t) = p1
                testTok (p1, p2, t) = test t

identifier :: DecafParser String
identifier = dtoken (\tok -> case tok of
                              Identf name -> Just name
                              other       -> Nothing)

reserved name = dtoken (\tok -> case tok of
                                  Reserv s | s == name -> Just ()
                                  other                  -> Nothing)

parser
