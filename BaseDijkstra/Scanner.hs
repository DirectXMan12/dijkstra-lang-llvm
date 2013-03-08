module BaseDijkstra.Scanner (GenDijkstraParser, keyword, symbol, identifier, typeKeyword, floatConst, intConst, boolConst) where
import Text.ParserCombinators.Parsec
import BaseDijkstra.Lexer

type GenDijkstraParser rt = GenParser Token SourcePos rt

djktoken :: (Tok -> Maybe a) -> GenDijkstraParser a
djktoken tokTest = do
  n <- getParserState
  setState (statePos n)
  token showToken posToken testToken
  where
    showToken (pos,tok) = show tok
    posToken (pos,_) = pos
    testToken (_,tok) = tokTest tok

keyword :: String -> GenDijkstraParser ()
keyword name = 
  djktoken (\tok -> case tok of
    Keyword k    | k == name -> Just ()
    other -> Nothing)

symbol :: String -> GenDijkstraParser ()
symbol name = 
  djktoken (\tok -> case tok of
    Symbol k    | k == name -> Just ()
    other -> Nothing)

typeKeyword :: GenDijkstraParser String
typeKeyword = 
  djktoken (\tok -> case tok of
    Type t -> Just t
    other -> Nothing)

identifier :: GenDijkstraParser String
identifier = 
  djktoken (\tok -> case tok of
    Identifier i -> Just i
    other -> Nothing)

floatConst :: GenDijkstraParser String
floatConst = 
  djktoken (\tok -> case tok of
    Float whole part -> Just (whole ++ "." ++ part)
    other -> Nothing)

intConst :: GenDijkstraParser String
intConst = 
  djktoken (\tok -> case tok of
    Integer whole -> Just whole
    other -> Nothing)

boolConst :: GenDijkstraParser String
boolConst = 
  djktoken (\tok -> case tok of
    Keyword "true" -> Just "true"
    Keyword "false" -> Just "false"
    other -> Nothing)

constant :: GenDijkstraParser String
constant = 
  djktoken (\tok -> case tok of
    Float whole part -> Just (whole ++ "." ++ part)
    Integer i -> Just i
    Symbol "true" -> Just "true"
    Symbol "false" -> Just "false"
    other -> Nothing)
