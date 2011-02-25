{- CHECK

-}

module Scanner where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Pos
import Text.Regex.Posix
import Monad
import Data.List

-- utility 

oneLine :: String -> String
oneLine = init.(foldl (++) "").(map (++" ")).lines



-- Token Def
data Tok = Charliteral String
         | Intliteral String
         | Booleanliteral String
         | Stringliteral String
         | Identifier String 
         | Reserved String
         | Error String

type Token = (SourcePos, Tok)

--type Parser a  = GenParser Char Int a

instance Show Tok where show = showTok


showTok (Charliteral s)    = "CHARLITERAL " ++ s
showTok (Intliteral s)     = "INTLITERAL " ++ s
showTok (Booleanliteral s) = "BOOLEANLITERAL " ++ s
showTok (Stringliteral s)  = "STRINGLITERAL " ++ "\"" ++ s ++ "\""
showTok (Identifier s)     = "IDENTIFIER " ++ s
showTok (Reserved s)       = s
showTok (Error s)          = s

showToken :: Token -> String
showToken (p, Error t) = oneLine t
showToken (p, t) = (show $sourceLine p) ++ " " ++ (show t)


reserved = ["boolean", "break", "callout", "class", "continue",
            "else", "for", "if", "int", "return", "void"]

reservedSym = ["-=", "+=", "==", "+", "-", "*", "/", "%", "<=", ">=",
            "<", ">", "!=", "&&", "||", "=", "{", "}", "[", "]",
            "(", ")", ",",";"]

illegal = ""


-- Scanner primitive methods

-- utility used for Char and String literals
pickChar :: Parser String
pickChar = (char '\\' >> oneOf "nt\\\"\'" >>= \s -> return ("\\"++[s]++""))
           <|> (noneOf "\\\'\"" >>= \c -> return $ [c])
           <|> (getInput >>= \c -> unexpected $show $head c)
           <?> "character"

spaces :: Parser ()
spaces = skipMany1 ((try (space >> eol)) <|> (space >> return ()) <|> try (string "//" >> comment))

comment :: Parser ()
comment = eol <|> (anyTokenInc >> comment)

eol :: Parser ()
eol = (char '\n' >> return ())
      <|> (char '\r' >> return ())
      <|> (eof)

parseChar :: Parser Token
parseChar = do char '\''
               x <- pickChar
               char '\''
               p <- getPosition
               return (p, Charliteral ("\'"++x++"\'"))

parseStr :: Parser Token
parseStr = do char '"'
              x <- many pickChar
              char '"'
              p <- getPosition
              return (p, Stringliteral $ foldl (++) "" x)

parseBool :: Parser Token
parseBool = do x <- string "true" <|> string "false"
               p <- getPosition
               return (p, Booleanliteral x)

parseHex :: Parser Token
parseHex = string "0x" >> getInput >>= \c -> (many1 hexDigit 
           <|> unexpected (show $head c) ) >>= 
           \x -> getPosition >>= \p -> return (p, Intliteral ("0x"++x))

parseInt :: Parser Token
parseInt = do x <- many1 digit
              p <- getPosition
              return (p, Intliteral x)

parseId :: Parser Token
parseId = do x <- letter <|> char '_'
             rest <- many (letter <|> digit <|> char '_') 
             whole <- return (x:rest)
             notFollowedBy $ oneOf illegal
             p <- getPosition
             return $if (x:rest `elem` reserved)
                    then (p, Reserved $x:rest)
                    else if (x:rest == "false" || x:rest == "true")
                         then  (p, Booleanliteral $x:rest)
                         else  (p, Identifier (x:rest))


parseResSym :: Parser Token
parseResSym = do x <- foldr (<|>) (fail "reserved error") (map (try.string) reservedSym)
                 p <- getPosition
                 return (p, Reserved x)

-- combines all the other token types
parseToken = parseId <|> parseResSym <|> parseChar
             <|> (try parseHex) <|> parseInt <|> parseStr 
             <|> (do eof; pos <- getPosition; return (pos, Error "eof"))
             <|> (do pos <- getPosition; 
                     c <- getInput;
                     return (incSourceColumn pos 1, Error (show pos++":\nUnknown Symbol "++ (show $head c))))


-- used to skip input after an error
applyNTimes :: GenParser tok st a -> Int -> GenParser tok st a
applyNTimes pattern n = foldr (>>) pattern (map (\_ -> pattern) [1..(n-1)])

anyTokenInc :: Parser Char
anyTokenInc = do c <- anyToken
                 pos <- getPosition
                 setPosition $updatePosChar pos c
                 return c
                               


skipToPos :: SourcePos -> Parser Char
skipToPos pos = let r = sourceLine pos - 1
                    c = sourceColumn pos - 1 in
                do case r of
                     0 -> return 'a'
                     _ -> applyNTimes (do (many (noneOf "\n\r")); char '\n') r 
                   case c of
                     0 -> return 'a'
                     _ -> applyNTimes anyTokenInc c


errorEOF :: ParseError -> Int
errorEOF err = if (show err =~ "unexpected end of file" :: Bool)
                     then 0
                     else 1


-- following 4 methods used for repeated token scanning
scanOneToken :: Parser Token -> Parser () -> String -> SourcePos -> Token
scanOneToken pattern spaces input pos = 
    case parse (skipToPos pos >> (many spaces) >> pattern) "" input of
      Left err  -> (incSourceColumn (errorPos err) (errorEOF err), Error $show err)
      Right val -> val

scanFirstToken :: Parser Token -> Parser () -> String ->  Token
scanFirstToken pattern spaces input = 
    case parse ((many spaces) >> pattern) "" input of
      Left err  -> (incSourceColumn (errorPos err) (errorEOF err), Error $show err)
      Right val -> val


beginScanString input = t : scanString cleaninput (fst t)
    where t = scanFirstToken parseToken spaces input 
          cleaninput = map fix input
          fix c = case c of
                    '\t' -> ' '
                    other -> other
                   
scanString input pos = thing
    where t = scanOneToken parseToken spaces input pos
          thing = case showTok $snd t of
                   "eof" -> []
                   otherwise -> t : scanString input (fst t)

                   
-- not used
readExpr :: String -> [Token]
readExpr input = case parse (sepBy parseToken spaces) "" input of
                   Left err -> [(errorPos err, Error $show err)]
                   Right val -> val


-- old main

--main :: IO ()
--main = getArgs >>= putStrLn.(foldl (++) "").(intersperse "\n").init.(map (showToken)).beginScanString.(!! 0)
--main = getArgs >>= putStrLn.show.doSpaces.(!! 0)
