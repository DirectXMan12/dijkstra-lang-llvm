module BaseDijkstra.Lexer (Token, Tok(..), DijkstraLexer, tokenizedDijkstra, lexDijkstra, lexDijkstraWithSrc) where
import Text.ParserCombinators.Parsec

type Token = (SourcePos, Tok)
data Tok
  = Keyword String
  | Type String
  | Identifier String
  | Integer String
  | Float String String
  | Symbol String deriving (Show, Eq)

type DijkstraLexer st = GenParser Char st Token

tokenizedDijkstra :: GenParser Char st [Token]
tokenizedDijkstra = do
  toks <- many1 (keyword <|> typeKeyword <|> identifier <|> numeric <|> sym)
  eof
  return toks

lexDijkstra :: String -> Either ParseError [Token]
lexDijkstra code = parse tokenizedDijkstra "(source unknown)" code

lexDijkstraWithSrc :: String -> String -> Either ParseError [Token]
lexDijkstraWithSrc code srcName = parse tokenizedDijkstra srcName code
  
lexeme p = do
  x <- p
  whiteSpace
  return x

symbol name = lexeme (string name)
whiteSpace = many (oneOf " \n\r\t")
reserved name =
  lexeme $ try $ do
    rn <- string name
    notFollowedBy letter <?> "end of " ++ show name
    return rn

symbolPrefixedByMaybe prefix rest =
  lexeme $ ((try $ do { rn1 <- string prefix; rn2 <- oneOf rest; return (rn1 ++ [rn2]) }) <|> string prefix)

reservedAgainst prefix rest letters = 
  lexeme $ try $ do
    rn1 <- string prefix
    notFollowedBy (oneOf letters) <?> "end of " ++ show prefix
    rn2 <- string rest
    return (rn1 ++ rn2)

keyword :: DijkstraLexer st
keyword = do
  st <- getParserState
  let sp = statePos st
  kw <- reserved "program"  <|> reserved "do" <|> reserved "od" <|> reserved "if" <|> reserved "fi" <|> reserved "input" <|> reserved "print" <|> reserved "true" <|> reserved "false" <|> reserved "mod" <|> reserved "div"
  return (sp, Keyword kw)

typeKeyword :: DijkstraLexer st
typeKeyword = do
  st <- getParserState
  let sp = statePos st
  tk <- reserved "float" <|> reserved "int" <|> reserved "bool"
  return (sp, Type tk)


identifier :: DijkstraLexer st
identifier = lexeme $ do
  st <- getParserState
  let sp = statePos st
  fl <- letter
  rest <- many (oneOf (['a'..'z'] ++ ['0'..'9'] ++ "_?"))
  return (sp, Identifier (fl : rest))

numeric :: DijkstraLexer st
numeric =
  lexeme $ ((try $ do
    st <- getParserState
    let sp = statePos st
    whole <- many1 digit
    char '.'
    part <- many1 digit
    return (sp, Float whole part))
  <|> do
    st <- getParserState
    let sp = statePos st
    whole <- many1 digit
    return (sp, Integer whole))
     

int :: DijkstraLexer st
int = do 
  st <- getParserState
  let sp = statePos st
  i <- lexeme (many1 digit)
  return (sp, Integer i)

float :: DijkstraLexer st
float = lexeme $ do
  st <- getParserState
  let sp = statePos st
  whole <- many1 digit
  char '.'
  part <- many1 digit
  return (sp, Float whole part)
  
sym :: DijkstraLexer st
sym = do
  st <- getParserState
  let sp = statePos st
  sv  <- symbol "{" <|> symbol "}" 
     <|> symbol "(" <|> symbol ")"
     <|> symbolPrefixedByMaybe "<" "-=" 
     <|> symbol ";" <|> symbol "," <|> symbol "::"
     <|> symbolPrefixedByMaybe ">" "=" 
     <|> symbol "=" <|> symbol "~=" <|> symbol "&" <|> symbol "|"
     <|> symbol "-" <|> symbol "+" <|> symbol "*" <|> symbol "/"
     <|> symbol "<-"
  return (sp, Symbol sv)
