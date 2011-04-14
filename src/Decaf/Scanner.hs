module Decaf.Scanner
where
import Text.ParserCombinators.Parsec
import Text.Regex.Posix
import Decaf.Tokens
import Test.QuickCheck

--------------------------------------------
-- interface functions
--------------------------------------------
--
-- type TokenD = (Token, String)  
type TokenD = Token


-- |The 'scanner' function takes and generates a corresponding list of Token
scanner :: String -> [TokenD]
scanner = eatFirst

scannerQc :: String -> Bool
scannerQc = (\s -> (length s == 0) || (length (scanner s) > 0))



-- |The 'scprint' function scans a given string and returns provides formatted output
scprint :: String -> String
scprint = formatScannerOutput . scanner



-- |The 'formatScannerOutput' function formats a list of scanned tokens into human readable format
formatScannerOutput :: [TokenD] -> String
formatScannerOutput = unlines . map showTokenD


showTokenD :: TokenD -> String
-- showTokenD (t, s) = showToken t --  ++ "  ::  " ++ show s
showTokenD = showToken


-- |The 'numLexErrorsIn' function calculates the number of errors in a list of Token
numLexErrorsIn :: [Token] -> Int
numLexErrorsIn tokens = numErrors tokens' 0
                        where
                          tokens' = map getToken tokens

                          
-- |The 'numErrors' function incrementally counts the number of errors in a list of DecafToken
numErrors :: [DecafToken] -> Int -> Int
numErrors (t:ts) count = numErrors ts err'
                         where
                           err' = err t
                           err (Fail _)  = count + 1
                           err _         = count
numErrors [] count = count

--------------------------------------------
-- general
--------------------------------------------
--
eatN :: Int -> Parser ()
eatN 0 = return ()
eatN n = anyChar >> eatN (n-1)

--------------------------------------------
-- whitespace
--------------------------------------------
--
eol :: Parser ()
eol = try eof <|> (char '\n' >> return ())
   <?> "end of line"

comment' :: Parser ()
comment' = eol <|> (eatN 1 >> comment')
        <?> "comment"

whitespace :: Parser ()
whitespace = try (string "//" >> comment') <|> (space >> return ())

ws :: Parser ()
ws = skipMany whitespace
  <?> "whitespace"

--------------------------------------------
-- literals
--------------------------------------------
--
genliteral :: Parser Token
genliteral = chrLiteral
       <|> strLiteral
       <|> numLiteral
       <?> "literal"

chrLiteral :: Parser Token
chrLiteral = do
                p1 <- getPosition
                char '\''
                c <- quoted
                char '\''
                p2 <- getPosition
                return ((p1, p2), CharLit c)
          <?> "character literal"

strLiteral :: Parser Token
strLiteral = do
              p1 <- getPosition
              char '"'
              s <- many quoted
              char '"'
              p2 <- getPosition
              return ((p1, p2), StrLit s)

quoted :: Parser Char
quoted = (char '\\' >> (oneOf "\\\'\""
                       <|> (char 'n' >> return '\n')
                       <|> (char 't' >> return '\t')
                       <?> "valid \\ char"))
         <|> noneOf "\"\'\t\n"   -- removed "\t\n", because \t and \n can be specified directly 
         <?> "quoted character"

numLiteral :: Parser Token
numLiteral = (do
                p1 <- getPosition
                try $ string "0x"
                d <- many1 hexDigit
                p2 <- getPosition
                return ((p1, p2), HexLit d))
          <|> (do
                 p1 <- getPosition
                 d <- many1 digit
                 p2 <- getPosition
                 return ((p1, p2), DecLit d))
          <?> "integer literal"

--------------------------------------------
-- identifiers
--------------------------------------------
--

makeIdentifier :: String -> DecafToken
makeIdentifier (t) = if t `elem` keywords
                     then if t `elem` bools
                          then if t == "true"
                             then BoolLit True
                             else BoolLit False
                          else Reserv t
                     else Identf t
                     where
                       bools = [
                         "true",
                         "false"
                         ]
                       keywords = [
                         "boolean",
                         "break",
                         "callout",
                         "class",
                         "continue",
                         "else",
                         "false",
                         "for",
                         "if",
                         "int",
                         "return",
                         "true",
                         "void"
                         ]


identifier :: Parser Token
identifier = do
                p1 <- getPosition
                h <- letter <|> char '_'
                r <- many (letter <|> digit <|> char '_')
                p2 <- getPosition
                let str = h : r
                return ((p1, p2), makeIdentifier str)
          <?> "identifier"

--------------------------------------------
-- operators
--------------------------------------------
--

operator :: Parser Token
operator = notOp
        <|> eqOp
        <|> condOp
        <|> relOp
        <|> arithOp
        <?> "operator"

notOp :: Parser Token
notOp = do
          p1 <- getPosition
          o <- try (string "!=") <|> string "!"
          p2 <- getPosition
          return ((p1, p2), mapOp o)
          where
            mapOp o | o == "!=" = OpNEq
                    | o == "!" = OpNot
            mapOp _ = error "Scanner.hs:180 unknown symbol passed to mapOp"

eqOp :: Parser Token
eqOp = do
        p1 <- getPosition
        o <- try (string "==") <|> string "="
        p2 <- getPosition
        return ((p1, p2), mapOp o)
        where
          mapOp o | o == "==" = OpEq
                  | o == "=" = Assign
          mapOp _ = error "Scanner.hs:190 unknown symbol passed to mapOp"

condOp :: Parser Token
condOp = do
          p1 <- getPosition
          o <- string "&&" <|> string "||"
          p2 <- getPosition
          return ((p1, p2), mapOp o)
          where
            mapOp o | o == "&&" = OpAnd
                    | o == "||" = OpOr
            mapOp _ = error "Scanner.hs:200 unknown symbol passed to mapOp"

arithOp :: Parser Token
arithOp = plusOp
       <|> minOp
       <|> unaryOp
             
unaryOp :: Parser Token
unaryOp = do
            p1 <- getPosition
            c <- oneOf "+-*/%"
            p2 <- getPosition
            return ((p1, p2), mapOp c)
            where
              mapOp c | c == '+' = OpAdd
                      | c == '-' = OpMin
                      | c == '*' = OpMul
                      | c == '/' = OpDiv
                      | c == '%' = OpMod
              mapOp _ = error "Scanner.hs:215 unknown symbol passed to mapOp"

plusOp :: Parser Token
plusOp = do
          p1 <- getPosition
          o <- try (string "+=") <|> string "+"
          p2 <- getPosition
          return ((p1, p2), mapOp o)
          where
            mapOp o | o == "+=" = PlusAssign
                    | o == "+" = OpAdd
            mapOp _ = error "Scanner.hs:228 unknown symbol passed to mapOp"

minOp :: Parser Token
minOp = do
          p1 <- getPosition
          o <- try (string "-=") <|> string "-"
          p2 <- getPosition
          return ((p1, p2), mapOp o)
          where
            mapOp o | o == "-=" = MinusAssign
                    | o == "-" = OpMin
            mapOp _ = error "Scanner.hs:238 unknown symbol passed to mapOp"

relOp :: Parser Token
relOp = do
          p1 <- getPosition
          o <- try (string "<=") <|> try (string ">=") <|> string "<" <|> string ">"
          p2 <- getPosition
          return ((p1, p2), mapOp o)
          where
            mapOp o | o == "<=" = OpLTE
                    | o == ">=" = OpGTE
                    | o == "<" = OpLT
                    | o == ">" = OpGT
            mapOp _ = error "Scanner.hs:248 unknown symbol passed to mapOp"
--------------------------------------------
-- terminals
--------------------------------------------
--
terminal :: Parser Token
terminal = do
          p1 <- getPosition
          b <- char ';' <|> char '[' <|> char ']' <|> char '(' <|> char ')' <|> char '{' <|> char '}' <|> char ','
          p2 <- getPosition
          return ((p1, p2), mapTerm b)
          where
            mapTerm b
                | b == ';' = Semi
                | b == '[' = LBrack
                | b == ']' = RBrack
                | b == '(' = LParen
                | b == ')' = RParen
                | b == '{' = LBrace
                | b == '}' = RBrace
                | b == ',' = Comma
            mapTerm _ = error "Scanner.hs:264 unknown symbol passed to mapTerm"

--------------------------------------------
-- Navigation
--------------------------------------------
--
--determine whether to start before or after the detected error
beforeOrAfter :: ParseError -> Int
beforeOrAfter e = if show e =~ "unexpected end of input"
                  then 0
                  else 1

eatPos :: String -> SourcePos -> Parser ()
eatPos input pos = eatN $ posCount input pos

posCount :: String -> SourcePos -> Int
posCount input p = let line = head $ lines input in
                     if sourceLine p == 1
                     then if sourceColumn p == 1
                          then 0
                          else 1 + posCount (tail input) (incSourceColumn p (-1))
                     else length line + 1 + posCount (unlines $ tail $ lines input) (incSourceLine p (-1))

singleToken :: Parser Token
singleToken = do
                ws
                t <- (operator <|> genliteral <|> identifier <|> terminal <|> end)
                ws
                return t


end :: Parser Token
end = do
        p1 <- getPosition
        eof
        p2 <- getPosition
        return ((p1, p2), EOF)

--------------------------------------------
-- scanner
--------------------------------------------
--



{-
eatNext :: Parser Token -> String -> [Token]
eatNext parser input = case parse parser "decaf-scanner-eatNext" input of
                        Left err -> let errPosition   = (errorPos err, incSourceColumn (errorPos err) $ 1 + beforeOrAfter err)
                                        errDecafToken = Fail $ show err
                                        errToken      = (errPosition, errDecafToken)
                                    in errToken : eatNext(failParser err) input

                        Right val -> if getToken val == EOF
                                     then []
                                     else val : eatNext (successParser val) input
                        where
                          failParser    e = eatPos input (incSourceColumn (errorPos e) $ beforeOrAfter e) >> singleToken
                          successParser v = eatPos input (getEnd v) >> singleToken

eatFirst :: String -> [Token]
eatFirst inp = case parse singleToken "decaf-scanner-eatFirst" input of
                  Left err -> let errPosition   = (errorPos err, incSourceColumn (errorPos err) $ 1 + beforeOrAfter err)
                                  errDecafToken = Fail $ show err
                                  errToken      = (errPosition, errDecafToken)
                              in errToken : eatNext(failParser err) input

                  Right val -> if getToken val == EOF
                               then []
                               else val : eatNext (successParser val) input
                  where
                    input = clean
                    clean = map fix inp
                    fix c | c == '\t' = ' '
                          | otherwise = c
                    failParser    e = eatPos input (incSourceColumn (errorPos e) $ beforeOrAfter e) >> singleToken
                    successParser e = eatPos input (getEnd e) >> singleToken
-}



-- scanner interface
        
        
eatFirst :: String -> [TokenD]
eatFirst str = scanString str 0 0


-- scan a string, return token stream, with position correted with the number of rows/cols scanned before the given string 
scanString :: String -> Int -> Int -> [TokenD]
scanString inp prow pcol = case parse singleToken "decaf-scanner-eatFirst" inp of
                  Left err -> 
                    let pos           = errorPos err
                        coepos        = adjTokenPos pos prow pcol   -- corrected error pos
                        coeendpos     = incSourceColumn coepos $ 1 + beforeOrAfter err   -- end pos
                        endpos        = incSourceColumn (errorPos err) $ 1 + beforeOrAfter err   -- uncorrected end pos
                        errPosition   = (coepos, coeendpos)
                        errDecafToken = Fail $ show err
                        errToken      = (errPosition, errDecafToken)
                        -- tok           = (errToken, inp)    -- debug
                        tok           = errToken 
                    in tok : asmNext endpos inp prow pcol

                  Right val -> if getToken val == EOF
                               then []
                               else let cval = ((adjTokenPos (getStart val) prow pcol, adjTokenPos (getEnd val) prow pcol), getToken val)
                                        -- tok  = (cval, (show $ positionCount inp (getEnd val) pcol) ++ "__" ++ inp)
                                        tok  = cval
                                    in tok : asmNext (getEnd val) inp prow pcol 

-- assemble the next 
asmNext :: SourcePos -> String -> Int -> Int -> [TokenD]
asmNext pos inp prow pcol = 
  let pc      = positionCount inp pos pcol  -- count #chars in scanned part
      splits  = splitAt pc inp          -- split 
      remain  =  snd $ splits           -- remaining part
      snrow   = foldr (\x y -> if x == '\n' || x == '\r' then (1+y) else y) 0 $ fst splits -- count number of rows scanned 
      slines  = lines $ fst splits      -- the scanned lines 
      sncol   = if snrow == 0 
                 then pcol + (length $ fst splits)   -- scanned within one line, inc col
                 else if length slines == snrow      -- token is at the beginning of last line, tricky case  "asdfsd/nasdfsd/nTOKEN" 
                      then 0 
                      else length $ last slines      -- otherwise the length of last line
      in scanString remain (prow + snrow) sncol




-- add token position, count in the effect that if the token is on a new line, then the previous col can be ignored
adjTokenPos :: SourcePos -> Int -> Int -> SourcePos
adjTokenPos pos prow pcol = if sourceLine pos > 1 then adjustLineCol pos prow 0 else adjustLineCol pos prow pcol
  

-- add previous line and col to sourcepos
adjustLineCol :: SourcePos -> Int -> Int -> SourcePos
adjustLineCol p l c = incSourceColumn (incSourceLine p l) c


-- count enough characters to reach sourcePos
positionCount :: String -> SourcePos -> Int -> Int               -- previous col
positionCount input p pcol = let line = head $ lines input in
  if (sourceLine p == 1) then countChar line (sourceColumn p) pcol
  else positionCount (unlines $ tail $ lines input) (incSourceLine p (-1)) 0 + (length line) + 1 



-- count actual #char in a string with Parsec's col count, with previous col skips pcol
countChar :: String -> Int -> Int -> Int 
countChar _ 1 _ = 0   -- don't want to delete last char
countChar (c:cs) col pcol | c == '\t' =
                            let skiped = 8-(pcol `mod` 8)            -- need to skip 8-(pcol `mod` 8) spaces   pcol=0 skip 8, pcol=1 7
                            in (countChar cs (col-skiped) (pcol+skiped)) + 1
                          | otherwise = (countChar cs (col-1) (pcol+1)) + 1   -- nahh
countChar [] _ _ = 0
countCharQc :: [Char] -> Bool
countCharQc = (\x -> let notab = "a" ++ foldr (\x y -> if x == '\t' then y else x:y) "" x 
	    in length notab - 1 == countChar notab (length notab) 0)




--tests
scannerQcTest :: IO ()
scannerQcTest = do
	    quickCheck scannerQc	    
	    quickCheck countCharQc

	    

