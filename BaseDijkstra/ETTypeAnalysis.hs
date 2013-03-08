module BaseDijkstra.ETTypeAnalysis (prog) where

import BaseDijkstra.TPMTest hiding (DjkTreeParser, prog)
import qualified BaseDijkstra.Parser as P (DijkstraTree(..))
import BaseDijkstra.Parser (NodeType(..))
import BaseDijkstra.NaryTreeZipper.Instances
import BaseDijkstra.MapTreeZipper
import qualified Data.IntMap as IntMap
import BaseDijkstra.TreePatternMatcher
import BaseDijkstra.SymbolTable hiding (VariableType(..))
import qualified BaseDijkstra.SymbolTable as ST (VariableType(..))
import BaseDijkstra.TypedDijkstraTree
import Data.Hashable

type SymTblZipper = MapTreeZipper SymbolTable (IntMap.IntMap VariableSymbol) P.DijkstraTree
type GenDjkTreeParser a = TreeParser P.DijkstraTree NodeType SymTblZipper a
type DjkTreeParser = GenDjkTreeParser TypedDijkstraTree

prog :: DjkTreeParser 
prog = do
  branchOfType PROGRAM
  _V_
  progName <- leafOfType ID
  visitNextSibling
  chldrn <- many1 progLine
  _A_
  return (BranchNode PROGRAM ((LeafNode ID progName ST.NOTAPP):chldrn) Nothing ST.NOTAPP)

progLine :: GenDjkTreeParser TypedDijkstraTree
progLine = decl <|> stmt

decl :: DjkTreeParser
decl = do
  (tp:names) <- branchOfType VARDECL <|-> (leafOfType TYPE <:> many1 (leafOfType ID) :: GenDjkTreeParser [[Char]])
  let nameNodes = map (\x -> LeafNode ID x ST.NOTAPP) names
  return (BranchNode VARDECL ((LeafNode TYPE tp ST.NOTAPP):nameNodes) Nothing ST.NOTAPP)

stmt :: DjkTreeParser
stmt = blockStmt <|> assignStmt <|> caseStmt <|> loopStmt <|> inputStmt <|> outputStmt

assignStmt :: DjkTreeParser
assignStmt = do
  branchOfType ASSIGN
  _V_
  names <- branchOfType VARS <|-> many1 (leafOfType ID)  
  visitNextSibling
  exprs <- branchOfType EXPRS <|-> many1 expr
  _A_
  currTblZip <- getUserState
  let leafIds = map (\x -> LeafNode ID x (lookupVarType currTblZip x)) names
  return (BranchNode ASSIGN [BranchNode VARS leafIds Nothing ST.NOTAPP, BranchNode EXPRS exprs Nothing ST.NOTAPP] Nothing ST.NOTAPP)

caseStmt :: DjkTreeParser
caseStmt = do
  guards <- branchOfType CASE <|-> many1 guardedStmt
  return (BranchNode CASE guards Nothing ST.NOTAPP)

guardedStmt :: DjkTreeParser
guardedStmt = do
  chldrn <- branchOfType WHERE <|-> (expr <+> stmt)
  return (BranchNode WHERE chldrn Nothing ST.NOTAPP)

loopStmt :: DjkTreeParser
loopStmt = do
  guards <- branchOfType LOOP <|-> many1 guardedStmt
  return (BranchNode LOOP guards Nothing ST.NOTAPP)

outputStmt :: DjkTreeParser
outputStmt = do
  ex <- branchOfType PRINT <|-> expr
  return (BranchNode PRINT [ex] Nothing ST.NOTAPP)

inputStmt :: DjkTreeParser
inputStmt = do
  ids <- branchOfType INPUT <|-> many1 (leafOfType ID)
  currTblZip <- getUserState
  let leafIds = map (\x -> LeafNode ID x (lookupVarType currTblZip x)) ids
  return (BranchNode INPUT leafIds Nothing ST.NOTAPP)

expr :: DjkTreeParser
expr = orExpr <|> andExpr <|> eqExpr <|> relExpr <|> addExpr <|> multExpr <|> unaryExpr <|> primaryExpr

echoWithType :: NodeType -> ST.VariableType -> DjkTreeParser
echoWithType nt vt = do
  exs <- branchOfType nt <|-> (expr <+> expr)
  return (BranchNode nt exs Nothing vt)

orExpr :: DjkTreeParser
orExpr = echoWithType OR ST.BOOL

andExpr :: DjkTreeParser
andExpr = echoWithType AND ST.BOOL

eqExpr :: DjkTreeParser
eqExpr = (echoWithType CMP_EQ ST.BOOL) <|> (echoWithType CMP_NEQ ST.BOOL)

relExpr :: DjkTreeParser
relExpr = (enb CMP_GT) <|> (enb CMP_LT) <|> (enb CMP_GE) <|> (enb CMP_LE)
  where enb t = echoWithType t ST.BOOL

addExpr :: DjkTreeParser
addExpr = do
  [lhs, rhs] <- (branchOfType PLUS <|> branchOfType MINUS) <|-> (expr <+> expr)
  node <- getCurrNode 
  let nt = P.treeType node
  let et = case typePair of (ST.FLOAT, ST.FLOAT) -> ST.FLOAT
                            (ST.FLOAT, ST.INT) -> ST.FLOAT
                            (ST.INT, ST.FLOAT) -> ST.FLOAT
                            (ST.INT, ST.INT) -> ST.INT
                            otherwise -> ST.NUMERIC
                            where typePair = (expressionType lhs, expressionType rhs)
  return (BranchNode nt [lhs,rhs] Nothing et)

multExpr :: DjkTreeParser
multExpr = do
  [lhs, rhs] <- (branchOfType MULT <|> branchOfType FDIV <|> branchOfType IDIV <|> branchOfType MOD) <|-> (expr <+> expr)
  node <- getCurrNode 
  let nt = P.treeType node
  let et = case typePair of (ST.FLOAT, ST.FLOAT) -> ST.FLOAT
                            (ST.FLOAT, ST.INT) -> ST.FLOAT
                            (ST.INT, ST.FLOAT) -> ST.FLOAT
                            (ST.INT, ST.INT) -> if nt == FDIV then ST.FLOAT else ST.INT
                            otherwise -> if nt == FDIV then ST.FLOAT else ST.NUMERIC
                            where typePair = (expressionType lhs, expressionType rhs)
  return (BranchNode nt [lhs,rhs] Nothing et)

unaryExpr :: DjkTreeParser
unaryExpr = negExpr <|> do { ex <- branchOfType LNOT <|-> expr; return (BranchNode LNOT [ex] Nothing ST.BOOL) }

negExpr :: DjkTreeParser
negExpr = do
  ex <- branchOfType NEG <|-> expr
  return (BranchNode NEG [ex] Nothing (expressionType ex))

primaryExpr :: DjkTreeParser
primaryExpr = constant <|> varLookup

varLookup :: DjkTreeParser
varLookup = do
  lid <- leafOfType ID
  currTbl <- getUserState
  return (LeafNode ID lid (lookupVarType currTbl lid))

constant :: DjkTreeParser
constant = echo1WithType FLOAT ST.FLOAT <|> echo1WithType INT ST.INT <|> echo1WithType TRUE ST.BOOL <|> echo1WithType FALSE ST.BOOL
  where echo1WithType nt vt = leafOfType nt >>= (\val -> return (LeafNode nt val vt))

blockStmt :: DjkTreeParser
blockStmt = do
  branchOfType BLOCK

  currNode <- getCurrNode
  
  _V_
  upperTbl <- getUserState
  setUserState $ mtToChild currNode upperTbl

  chldrn <- many1 progLine
  _A_
  
  lowerTbl <- getUserState
  setUserState $ mtToUp lowerTbl

  return (BranchNode BLOCK chldrn Nothing ST.NOTAPP)

lookupVarType :: SymTblZipper -> String -> ST.VariableType
lookupVarType zip@((currNode, (SymbolTable vars chldrn)), parCrumb:rest) varName =
  case IntMap.lookup (hash varName) vars of Just vt -> varType vt
                                            Nothing -> lookupVarType (mtToUp zip) varName

lookupVarType zip@((currNode, (SymbolTable vars chldrn)), []) varName =
  case IntMap.lookup (hash varName) vars of Just vt -> varType vt
                                            Nothing -> error ("Couldn't look up variable with name " ++ varName ++ " in highest level of symbol table")
