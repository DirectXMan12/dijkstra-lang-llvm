{-# LANGUAGE DeriveGeneric #-}
module BaseDijkstra.Parser (parseDijkstra, DijkstraTree(..), NodeType(..)) where
import Text.ParserCombinators.Parsec
import Text.Parsec.Pos
import BaseDijkstra.Scanner
import BaseDijkstra.Lexer (lexDijkstra)
import GHC.Generics (Generic)
import Data.Hashable

data NodeType
  = PROGRAM 
  | FLOAT | INT | TRUE | FALSE 
  | ID 
  | ASSIGN | LOOP | CASE | WHERE | INPUT | PRINT | BLOCK 
  | OR | AND | CMP_EQ | CMP_NEQ | CMP_GT | CMP_LT | CMP_LE | CMP_GE | LNOT 
  | NEG | PLUS | MINUS | MULT | FDIV | IDIV | MOD 
  | VARS | EXPRS | TYPE | CAST
  | VARDECL deriving (Show, Eq, Enum, Bounded, Ord, Generic)

instance Hashable NodeType
 
instance Hashable SourcePos where
  hashWithSalt salt sp = salt `hashWithSalt` (sourceName sp) `hashWithSalt` (sourceLine sp) `hashWithSalt` (sourceColumn sp)

data DijkstraTree = BranchNode {treeType :: NodeType, children :: [DijkstraTree], posInfo :: Maybe SourcePos} | LeafNode {treeType :: NodeType, value :: String} deriving (Eq, Generic)

instance Hashable DijkstraTree

instance Show DijkstraTree where
  show (LeafNode nt val) = "(" ++ (show nt) ++ " " ++ val ++ ")"
  show (BranchNode nt cs Nothing) = "(" ++ (show nt) ++ " " ++ (unwords $ map show cs) ++ ")"
  show (BranchNode nt cs (Just pos)) = "(" ++ (show nt) ++ "[" ++ show pos ++ "] " ++ (unwords $ map show cs) ++ ")"

type CharGenParser a st = GenParser Char st a
type DijkstraParser = GenDijkstraParser DijkstraTree

parseDijkstra :: String -> Either ParseError DijkstraTree
parseDijkstra code = (lexDijkstra code) >>= (runParser program (initialPos "(source unknown)") "(source unknown)")

program :: DijkstraParser
program = do
  keyword "program"
  progName <- identifier
  sep
  lines <- many sepline
  let newDjkTree = LeafNode ID progName
  let newArray = newDjkTree : lines
  return (BranchNode PROGRAM newArray Nothing)

sep :: GenDijkstraParser ()
sep = skipMany (symbol ";")

line :: DijkstraParser
line = decl <|> stmt

sepline :: DijkstraParser
sepline = do
  l <- line
  sep
  return l

decl :: DijkstraParser
decl = do
  varType <- typeKeyword
  varNames <- sepBy identifier comma
  let vnLeaves = map (\x -> LeafNode ID x) varNames
  return (BranchNode VARDECL ((LeafNode TYPE varType):vnLeaves) Nothing)

stmt :: DijkstraParser
stmt = blockStmt <|> assignStmt <|> caseStmt <|> loopStmt <|> inputStmt <|> outputStmt

blockStmt :: DijkstraParser
blockStmt = do
  pos <- getState
  symbol "{"
  stmts <- many sepline 
  symbol "}"
  return (BranchNode BLOCK stmts (Just pos))

comma :: GenDijkstraParser ()
comma = symbol ","

assignStmt :: DijkstraParser
assignStmt = do
  ids <- sepBy identifier comma
  symbol "<-"
  exprs <- sepBy expr comma
  let leafIds = map (\x -> LeafNode ID x) ids
  return (BranchNode ASSIGN [BranchNode VARS leafIds Nothing, BranchNode EXPRS exprs Nothing] Nothing)
  
loopStmt :: DijkstraParser
loopStmt = do
  keyword "do"
  res <- many guardedStmt
  keyword "od"
  return (BranchNode LOOP res Nothing)
  
caseStmt :: DijkstraParser
caseStmt = do
  keyword "if"
  res <- many guardedStmt
  keyword "fi"
  return (BranchNode CASE res Nothing)

guardedStmt :: DijkstraParser
guardedStmt = do
  gEx <- expr
  symbol "::"
  cStmt <- stmt
  return (BranchNode WHERE [gEx,cStmt] Nothing)
  
inputStmt :: DijkstraParser
inputStmt = do
  keyword "input"
  ids <- sepBy identifier comma
  let leafIds = map (\x -> LeafNode ID x) ids
  return (BranchNode INPUT leafIds Nothing)
  
outputStmt :: DijkstraParser
outputStmt = do
  keyword "print"
  ex <- expr
  return (BranchNode PRINT [ex] Nothing)

expr :: DijkstraParser
-- expr = (float <|> int <|> bool) >>= (\x -> return x)
expr = orExpr

orExpr :: DijkstraParser
orExpr = chainl1 andExpr orOp

--orOp :: GenDijkstraParser (DijkstraTree -> DijkstraTree -> DijkstraTree)
orOp = do
  symbol "|"
  return (\x y -> BranchNode OR [x,y] Nothing)

andExpr :: DijkstraParser
andExpr = chainl1 eqExpr andOp

andOp = do
  symbol "&"
  return (\x y -> BranchNode AND [x,y] Nothing)

eqExpr :: DijkstraParser
eqExpr = chainr1 relExpr eqOp

eqOp 
   =  do { symbol "="; return (\x y -> BranchNode CMP_EQ [x,y] Nothing) } 
  <|> do { symbol "~="; return (\x y -> BranchNode CMP_NEQ [x,y] Nothing) }

relExpr :: DijkstraParser
relExpr = chainl1 addExpr relOp

relOp 
   =  do { symbol ">"; return (\x y -> BranchNode CMP_GT [x,y] Nothing) } 
  <|> do { symbol "<"; return (\x y -> BranchNode CMP_LT [x,y] Nothing) }
  <|> do { symbol "<="; return (\x y -> BranchNode CMP_LE [x,y] Nothing) }
  <|> do { symbol ">="; return (\x y -> BranchNode CMP_GE [x,y] Nothing) }

addExpr :: DijkstraParser
addExpr = chainl1 multExpr addOp

addOp 
   =  do { symbol "+"; return (\x y -> BranchNode PLUS [x,y] Nothing) } 
  <|> do { symbol "-"; return (\x y -> BranchNode MINUS [x,y] Nothing) }

multExpr :: DijkstraParser
multExpr = chainl1 unaryExpr multOp

multOp 
   =  do { symbol "*"; return (\x y -> BranchNode MULT [x,y] Nothing) } 
  <|> do { symbol "/"; return (\x y -> BranchNode FDIV [x,y] Nothing) }
  <|> do { keyword "div"; return (\x y -> BranchNode IDIV [x,y] Nothing) }
  <|> do { keyword "mod"; return (\x y -> BranchNode MOD [x,y] Nothing) }

unaryExpr :: DijkstraParser
unaryExpr = do { f <- unaryOp; r <- unaryExpr; return (f r) } <|> primaryExpr

unaryOp 
   =  do { symbol "-"; return (\x -> BranchNode NEG [x] Nothing) } 
  <|> do { symbol "~"; return (\x -> BranchNode LNOT [x] Nothing) }

primaryExpr :: DijkstraParser
primaryExpr = constant <|> varLookup <|> parenExpr

constant :: DijkstraParser
constant = float <|> int <|> bool

varLookup :: DijkstraParser
varLookup = identifier >>= (\x -> return (LeafNode ID x))

parenExpr :: DijkstraParser
parenExpr = do
  symbol "("
  res <- expr
  symbol ")"
  return res

{- relExpr -> additiveExpr | additiveExpr relOp additiveExpr, additiveExpr -> multExpr | additiveExpr addOp multExpr, multExpr -> unaryExpr | multExpr multOp unaryExpr, unaryExpr -> ... -}

float :: DijkstraParser
float = floatConst >>= (\num -> return (LeafNode FLOAT num))

int :: DijkstraParser
int = intConst >>= (\num -> return (LeafNode INT num))

bool :: DijkstraParser
bool = do
  str <- boolConst
  case str of
    "true" -> return (LeafNode TRUE "true")
    other -> return (LeafNode FALSE "false")
