module BaseDijkstra.ETCastPass (prog) where

import BaseDijkstra.TPMTest hiding (DjkTreeParser, prog)
import qualified BaseDijkstra.Parser as P (DijkstraTree(..))
import BaseDijkstra.Parser (NodeType(..))
import BaseDijkstra.NaryTreeZipper.Instances
import BaseDijkstra.NaryTreeZipper
import qualified BaseDijkstra.MapTreeZipper as MTZ
import qualified Data.IntMap as IntMap
import BaseDijkstra.TreePatternMatcher hiding ((<|->))
import BaseDijkstra.SymbolTable hiding (VariableType(..))
import qualified BaseDijkstra.SymbolTable as ST (VariableType(..))
import BaseDijkstra.TypedDijkstraTree hiding (children)
import Data.Hashable

type GenDjkTreeParser a = TreeParser TypedDijkstraTree NodeType () a
type DjkTreeParser = GenDjkTreeParser TypedDijkstraTree

restoreType :: ST.VariableType -> GenDjkTreeParser ()
restoreType tp = TreeParser (\(BranchNode tt cs pos _, crumbs) st -> DidNothing (BranchNode tt cs pos tp, crumbs) st)

(<|->) :: GenDjkTreeParser a -> GenDjkTreeParser b -> GenDjkTreeParser b
ptp <|-> ctp = do
  ptp
  cn <- getCurrNode
  let cnEt = expressionType cn
  _V_
  res <- ctp
  _A_
  restoreType cnEt
  return res

-- WARNING: _V_ _A_ destroys expression type! Use carefully! The custom (<|->) above preserves expression type

-- IDEA: write a functions which "dives" to expr, echoing what it finds until it reaches a node that has a type that is not just NOTAPP, then does any transformations specified.
-- IDEA: transformations are specified by functions that transform a single attribute, and are composed together with a base function which just echos out the node.  This would probably just be a monoid of some sort (or monad plus), so something like `matcher <guardsymboloperator> replaceType xyz `mappend` wrapWithNode blah`

prog :: DjkTreeParser
prog = do
  branchOfType PROGRAM

  _V_
  progName <- leafOfType ID
  visitNextSibling
  cldrn <- many1 progLine
  _A_

  getCurrNode

progLine :: DjkTreeParser
progLine = expr <|> anystmt

anystmt :: DjkTreeParser
anystmt = assignStmt <|> anybn <|> anyln

anybn = (anyBranch <|-> (many1 progLine)) >> getCurrNode

anyln = do
  anyLeaf
  getCurrNode

anyLeaf :: GenDjkTreeParser (NodeType, String)
anyLeaf =
  TreeParser (\(tree,crumbs) st ->
    case tree of
      LeafNode lt lv _ -> Ok (tree, crumbs) st (lt,lv)
      BranchNode _ _ _ _ -> Error (tree, crumbs) st ("Could not match any leaf"))

anyBranch :: GenDjkTreeParser NodeType
anyBranch =
  TreeParser (\(tree,crumbs) st ->
    case tree of
      BranchNode bt _ _ _ -> Ok (tree, crumbs) st bt
      LeafNode _ _ _ -> Error (tree, crumbs) st ("Could not match any branch"))


expr :: DjkTreeParser
expr = orExpr <|> andExpr <|> cmpExpr <|> addExpr <|> multExpr <|> unaryExpr <|> primaryExpr

echoWithType :: NodeType -> ST.VariableType -> DjkTreeParser
echoWithType nt vt = do
  exs <- branchOfType nt <|-> (expr <+> expr)
  getCurrNode

orExpr :: DjkTreeParser
orExpr = echoWithType OR ST.BOOL

andExpr :: DjkTreeParser
andExpr = echoWithType AND ST.BOOL

cmpExpr :: DjkTreeParser
cmpExpr = do
  (lhs:rhs:[]) <- (cmpBranchTypes) <|-> (expr <+> expr)
  if (expressionType lhs /= expressionType rhs) 
    then error "You must compare two expressions of the same type -- no casting is allowed here!"
    else getCurrNode

cmpBranchTypes :: DjkTreeParser
cmpBranchTypes = ((branchOfType CMP_EQ) <|> (branchOfType CMP_NEQ) <|> (branchOfType CMP_GT) <|> (branchOfType CMP_LT) <|> (branchOfType CMP_GE) <|> (branchOfType CMP_LE)) >> getCurrNode

castLhsTo :: ST.VariableType -> DjkTreeParser
castLhsTo varType =
  TreeParser (\(tree, crumbs) st ->
    let
      [lhs, rhs] = children tree
      tt = nodeValue tree
      oldType = expressionType tree
      newLhs = BranchNode CAST [LeafNode TYPE (show varType) ST.NOTAPP, lhs] Nothing varType
      newTree = BranchNode tt [newLhs, rhs] Nothing oldType
    in Ok (newTree, crumbs) st newTree)

castRhsTo :: ST.VariableType -> DjkTreeParser
castRhsTo varType =
  TreeParser (\(tree, crumbs) st ->
    let
      [lhs, rhs] = children tree
      tt = nodeValue tree
      oldType = expressionType tree
      newRhs = BranchNode CAST [LeafNode TYPE (show varType) ST.NOTAPP, rhs] Nothing varType
      newTree = BranchNode tt [lhs, newRhs] Nothing oldType
    in Ok (newTree, crumbs) st newTree)

replaceCurrNode :: TypedDijkstraTree -> GenDjkTreeParser ()
replaceCurrNode nd = TreeParser (\(tree, crumbs) st -> DidNothing (nd, crumbs) st)

assignStmt :: DjkTreeParser
assignStmt = do
  branchOfType ASSIGN
  _V_
  leafIds <- branchOfType VARS <|-> many1 (do { leafOfType ID; getCurrNode })
  visitNextSibling
  exprs <- branchOfType EXPRS <|-> many1 expr
  _A_
  let assignPairs = zip leafIds exprs
  let finalExprs = map (\(LeafNode ID name tp, ex) -> if tp /= expressionType ex then (BranchNode CAST [LeafNode TYPE (show tp) ST.NOTAPP, ex] Nothing tp) else ex) assignPairs
  let newNode = (BranchNode ASSIGN [BranchNode VARS leafIds Nothing ST.NOTAPP, BranchNode EXPRS finalExprs Nothing ST.NOTAPP] Nothing ST.NOTAPP)
  replaceCurrNode newNode
  return newNode
    
addExpr :: DjkTreeParser
addExpr = do 
  [lhs, rhs] <- (branchOfType PLUS <|> branchOfType MINUS) <|-> (expr <+> expr)
  let typePair = (expressionType lhs, expressionType rhs)
  case typePair of (ST.FLOAT, ST.INT) -> castRhsTo ST.FLOAT >> getCurrNode
                   (ST.INT, ST.FLOAT) -> castLhsTo ST.FLOAT >> getCurrNode
                   otherwise -> getCurrNode

multExpr :: DjkTreeParser
multExpr = fmultExpr <|> imultExpr

fmultExpr :: DjkTreeParser
fmultExpr = do
  [lhs, rhs] <- (branchOfType MULT <|> branchOfType FDIV) <|-> (expr <+> expr)
  let typePair = (expressionType lhs, expressionType rhs)
  cn <- getCurrNode
  let nt = nodeType cn
  case typePair of (ST.FLOAT, ST.INT) -> castRhsTo ST.FLOAT >> getCurrNode
                   (ST.INT, ST.FLOAT) -> castLhsTo ST.FLOAT >> getCurrNode
                   (ST.INT, ST.INT) -> if nt == FDIV then castLhsTo ST.FLOAT >> castRhsTo ST.FLOAT >> getCurrNode else getCurrNode
                   otherwise -> getCurrNode

imultExpr :: DjkTreeParser
imultExpr = do
  [lhs,rhs] <- (branchOfType IDIV <|> branchOfType MOD) <|-> (expr <+> expr)
  if (expressionType lhs /= ST.INT || expressionType rhs /= ST.INT)
    then error "Cannot use the `mod` and `div` operators on anything other than int, and no casting is allowed!"
    else getCurrNode

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
  leafOfType ID
  getCurrNode

constant :: DjkTreeParser
constant = echo1WithType FLOAT ST.FLOAT <|> echo1WithType INT ST.INT <|> echo1WithType TRUE ST.BOOL <|> echo1WithType FALSE ST.BOOL
  where echo1WithType nt vt = leafOfType nt >>= (\val -> return (LeafNode nt val vt))
